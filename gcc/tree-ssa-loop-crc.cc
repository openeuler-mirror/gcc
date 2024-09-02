/* This pass converts special loops where do CRC algorithms to
   simple CRC instructions in AArch64.
   Copyright (C) 2023-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "ssa.h"
#include "tree-into-ssa.h"
#include "cfganal.h"
#include "cfgloop.h"
#include "gimple-pretty-print.h"
#include "tree-cfg.h"
#include "cgraph.h"
#include "print-tree.h"
#include "cfghooks.h"
#include "gimple-fold.h"
#include "diagnostic-core.h"

/* This pass handles scenarios similar to the following:
ulg updcrc (s, n)
    uch *s;
    unsigned n;
{
    register ulg c;

    static ulg crc = (ulg)0xffffffffL; 

    if (s == NULL)
    {
        c = 0xffffffffL;
    }
    else
    {
        c = crc;
        if (n)
        do
        {
            c = crc_32_tab[((int)c ^ (*s++)) & 0xff] ^ (c >> 8);
        } while (--n);
    }
    crc = c;
    return c ^ 0xffffffffL;
}

If the hardware supports the crc instruction, then the pass completes the
conversion of the above scenario into:

#define SIZE_U32 sizeof(uint32_t)
unsigned long updcrc(s, n)
    unsigned char *s;
    unsigned n;
{
  register unsigned long c;

  static unsigned long crc = (unsigned long)0xffffffffL;

  if (s == NULL)
  {
    c = 0xffffffffL;
  }
  else
  {
    c = crc;
    if (n)
    {
      uint32_t nn = n/SIZE_U32;
      do
      {
        c = __crc32w (c,*((uint32_t *)s));
        s += SIZE_U32;
      } while(--nn);
    }
  }
  if (n & sizeof (uint16_t))
  {
    c = __crc32h (c, *((uint16_t *)s));
    s += sizeof (uint16_t);
  }
  if (n & sizeof (uint8_t))
    c = __crc32b (c, *s);
  crc = c;
  return c ^ 0xffffffffL;
}

This pass is to complete the conversion of such scenarios from
the internal perspective of the compiler:
1) match_crc_loop: The function completes the screening of such
   scenarios;
2) convert_to_new_loop: The function completes the conversion of
   origin_loop to new loops, and removes origin_loop;
3) origin_loop_info: The structure is used to record important
   information of origin_loop: such as loop exit, initial value
   of induction variable, etc;
4) create_new_loops: The function is used as the key content
   of the pass to complete the creation of new loops.  */

extern bool gimple_crc_match_index (tree, tree *, tree (*)(tree));
extern bool gimple_crc_match_res (tree, tree *, tree (*)(tree));

static gimple *crc_table_read_stmt = NULL;

static gphi *phi_s = NULL;
static gphi *phi_c = NULL;
static tree nn_tree = NULL;

enum aarch64_crc_builtins
{
  AARCH64_BUILTIN_CRC32B,
  AARCH64_BUILTIN_CRC32H,
  AARCH64_BUILTIN_CRC32W,
};

/* The useful information of origin loop.  */
struct origin_loop_info
{
  tree limit;       /* The limit index of the array in the old loop.  */
  tree base_n;      /* The initial value of the old loop.  */
  tree base_s;      /* The initial value of the old loop.  */
  tree base_c;      /* The initial value of the old loop.  */
  edge entry_edge;  /* The edge into the old loop.  */
  edge exit_edge;   /* The edge outto the old loop.  */
  basic_block exit_bb;
};

typedef struct origin_loop_info origin_loop_info;

static origin_loop_info origin_loop;
hash_map <basic_block, tree> n_map;
hash_map <basic_block, tree> nn_map;
hash_map <basic_block, tree> s_map;
hash_map <basic_block, tree> c_map;
hash_map <basic_block, tree> crc_map;

/* Initialize the origin_loop structure.  */
static void
init_origin_loop_structure ()
{
  origin_loop.entry_edge = NULL;
  origin_loop.exit_edge = NULL;
  origin_loop.exit_bb = NULL;
  origin_loop.limit = NULL;
  origin_loop.base_n = NULL;
  origin_loop.base_s = NULL;
  origin_loop.base_c = NULL;
}

/* Get the edge that first entered the loop.  */
static edge
get_loop_preheader_edge (class loop *loop)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, loop->header->preds)
    if (e->src != loop->latch)
      break;

  return e;
}

/* Returns true if t is SSA_NAME and user variable exists.  */

static bool
ssa_name_var_p (tree t)
{
  if (!t || TREE_CODE (t) != SSA_NAME)
    return false;
  if (SSA_NAME_VAR (t))
    return true;
  return false;
}

/* Returns true if t1 and t2 are SSA_NAME and belong to the same variable.  */

static bool
same_ssa_name_var_p (tree t1, tree t2)
{
  if (!ssa_name_var_p (t1) || !ssa_name_var_p (t2))
    return false;
  if (SSA_NAME_VAR (t1) == SSA_NAME_VAR (t2))
    return true;
  return false;
}

/* Get origin loop induction variable upper bound.  */

static bool
get_iv_upper_bound (gimple *stmt)
{
  if (origin_loop.limit != NULL || origin_loop.base_n != NULL)
    return false;

  tree lhs = gimple_cond_lhs (stmt);
  tree rhs = gimple_cond_rhs (stmt);

  if (TREE_CODE (TREE_TYPE (lhs)) != INTEGER_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) != INTEGER_TYPE)
    return false;

  /* TODO: Currently, the input restrictions on lhs and rhs are implemented
     through PARM_DECL.  We may consider relax the restrictions later, and
     we need to consider the overall adaptation scenario and adding test
     cases.  */
  if (ssa_name_var_p (lhs) && TREE_CODE (SSA_NAME_VAR (lhs)) == PARM_DECL)
  {
    origin_loop.limit = rhs;
    origin_loop.base_n = lhs;
  }
  else
    return false;

  if (origin_loop.limit != NULL && origin_loop.base_n != NULL)
    return true;

  return false;
}

/* Get origin loop info.  */
static bool
get_origin_loop_info (class loop *loop)
{
  auto_vec<edge> edges = get_loop_exit_edges (loop);
  origin_loop.exit_edge = edges[0];
  origin_loop.exit_bb = origin_loop.exit_edge->dest;
  origin_loop.entry_edge = get_loop_preheader_edge (loop);
  origin_loop.base_s = PHI_ARG_DEF_FROM_EDGE (phi_s,origin_loop.entry_edge);
  origin_loop.base_c = PHI_ARG_DEF_FROM_EDGE (phi_c,origin_loop.entry_edge);

  basic_block preheader_bb = origin_loop.entry_edge->src;

  if (preheader_bb->preds->length () != 1)
    return false;

  edge entry_pre_bb_edge = EDGE_PRED (preheader_bb, 0);

  basic_block pre_preheader_bb = entry_pre_bb_edge->src;

  gimple_stmt_iterator gsi;
  gimple *stmt;
  bool get_upper_bound = false;
  for (gsi = gsi_start_bb (pre_preheader_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      if (stmt && gimple_code (stmt) == GIMPLE_COND
          && get_iv_upper_bound (stmt))
        {
          get_upper_bound = true;
          break;
        }
    }

  return get_upper_bound;
}

/* The loop form check will check the entire loop control flow
   It should be a loop that:
   1. a do-while loop with header and latch only with no other control flow
      inside the loop
   2. have only one exiting edge
   3. have only one back edge and one entry edge
*/
static bool
crc_loop_form_check (class loop *loop)
{
  if (loop->num_nodes > 2 || loop->inner)
    return false;
  // Should only have 1 exit edge
  auto_vec<edge> edges = get_loop_exit_edges (loop);
  if (edges.length() != 1)
    return false;

  // The header should have only 2 incoming edges
  // One of them is the preheader edge and the other is the backedge from the
  // latch
  if (EDGE_COUNT (loop->header->preds) != 2)
    return false;
  edge e1 = EDGE_PRED (loop->header, 0);
  edge e2 = EDGE_PRED (loop->header, 1);

  if ((e1->src == loop->latch && e2->src->loop_father != loop)
      || (e2->src == loop->latch && e1->src->loop_father != loop))
    return true;

  return false;
}

/* Check there is only one array is read in the loop.
   Return the only array as crc_table.  */
static bool
only_one_array_read (class loop *loop, tree &crc_table)
{
  gimple_stmt_iterator gsi;
  gimple *stmt;
  bool res = false;
  for (gsi = gsi_start_bb (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      if (stmt == NULL)
        return false;

      if (gimple_code (stmt) == GIMPLE_ASSIGN
          && TREE_CODE (gimple_assign_lhs (stmt)) == ARRAY_REF)
        return false;

      /* Only one-dimensional integer arrays meet the condition.  */
      if (gimple_code (stmt) == GIMPLE_ASSIGN
          && TREE_CODE (gimple_assign_rhs1 (stmt)) == ARRAY_REF
          && TREE_CODE (TREE_OPERAND (gimple_assign_rhs1 (stmt), 0)) == VAR_DECL
          && TREE_CODE (TREE_TYPE (gimple_assign_rhs1 (stmt))) == INTEGER_TYPE)
        {
          if (crc_table == NULL
              && TREE_READONLY (gimple_assign_rhs1 (stmt)))
            {
              crc_table = gimple_assign_rhs1 (stmt);
              crc_table_read_stmt = stmt;
              res = true;
            }
          else
            return false;
        }
    }
  return res;
}

static const unsigned HOST_WIDE_INT crc_32_tab[] = {
  0x00000000L, 0x77073096L, 0xee0e612cL, 0x990951baL, 0x076dc419L,
  0x706af48fL, 0xe963a535L, 0x9e6495a3L, 0x0edb8832L, 0x79dcb8a4L,
  0xe0d5e91eL, 0x97d2d988L, 0x09b64c2bL, 0x7eb17cbdL, 0xe7b82d07L,
  0x90bf1d91L, 0x1db71064L, 0x6ab020f2L, 0xf3b97148L, 0x84be41deL,
  0x1adad47dL, 0x6ddde4ebL, 0xf4d4b551L, 0x83d385c7L, 0x136c9856L,
  0x646ba8c0L, 0xfd62f97aL, 0x8a65c9ecL, 0x14015c4fL, 0x63066cd9L,
  0xfa0f3d63L, 0x8d080df5L, 0x3b6e20c8L, 0x4c69105eL, 0xd56041e4L,
  0xa2677172L, 0x3c03e4d1L, 0x4b04d447L, 0xd20d85fdL, 0xa50ab56bL,
  0x35b5a8faL, 0x42b2986cL, 0xdbbbc9d6L, 0xacbcf940L, 0x32d86ce3L,
  0x45df5c75L, 0xdcd60dcfL, 0xabd13d59L, 0x26d930acL, 0x51de003aL,
  0xc8d75180L, 0xbfd06116L, 0x21b4f4b5L, 0x56b3c423L, 0xcfba9599L,
  0xb8bda50fL, 0x2802b89eL, 0x5f058808L, 0xc60cd9b2L, 0xb10be924L,
  0x2f6f7c87L, 0x58684c11L, 0xc1611dabL, 0xb6662d3dL, 0x76dc4190L,
  0x01db7106L, 0x98d220bcL, 0xefd5102aL, 0x71b18589L, 0x06b6b51fL,
  0x9fbfe4a5L, 0xe8b8d433L, 0x7807c9a2L, 0x0f00f934L, 0x9609a88eL,
  0xe10e9818L, 0x7f6a0dbbL, 0x086d3d2dL, 0x91646c97L, 0xe6635c01L,
  0x6b6b51f4L, 0x1c6c6162L, 0x856530d8L, 0xf262004eL, 0x6c0695edL,
  0x1b01a57bL, 0x8208f4c1L, 0xf50fc457L, 0x65b0d9c6L, 0x12b7e950L,
  0x8bbeb8eaL, 0xfcb9887cL, 0x62dd1ddfL, 0x15da2d49L, 0x8cd37cf3L,
  0xfbd44c65L, 0x4db26158L, 0x3ab551ceL, 0xa3bc0074L, 0xd4bb30e2L,
  0x4adfa541L, 0x3dd895d7L, 0xa4d1c46dL, 0xd3d6f4fbL, 0x4369e96aL,
  0x346ed9fcL, 0xad678846L, 0xda60b8d0L, 0x44042d73L, 0x33031de5L,
  0xaa0a4c5fL, 0xdd0d7cc9L, 0x5005713cL, 0x270241aaL, 0xbe0b1010L,
  0xc90c2086L, 0x5768b525L, 0x206f85b3L, 0xb966d409L, 0xce61e49fL,
  0x5edef90eL, 0x29d9c998L, 0xb0d09822L, 0xc7d7a8b4L, 0x59b33d17L,
  0x2eb40d81L, 0xb7bd5c3bL, 0xc0ba6cadL, 0xedb88320L, 0x9abfb3b6L,
  0x03b6e20cL, 0x74b1d29aL, 0xead54739L, 0x9dd277afL, 0x04db2615L,
  0x73dc1683L, 0xe3630b12L, 0x94643b84L, 0x0d6d6a3eL, 0x7a6a5aa8L,
  0xe40ecf0bL, 0x9309ff9dL, 0x0a00ae27L, 0x7d079eb1L, 0xf00f9344L,
  0x8708a3d2L, 0x1e01f268L, 0x6906c2feL, 0xf762575dL, 0x806567cbL,
  0x196c3671L, 0x6e6b06e7L, 0xfed41b76L, 0x89d32be0L, 0x10da7a5aL,
  0x67dd4accL, 0xf9b9df6fL, 0x8ebeeff9L, 0x17b7be43L, 0x60b08ed5L,
  0xd6d6a3e8L, 0xa1d1937eL, 0x38d8c2c4L, 0x4fdff252L, 0xd1bb67f1L,
  0xa6bc5767L, 0x3fb506ddL, 0x48b2364bL, 0xd80d2bdaL, 0xaf0a1b4cL,
  0x36034af6L, 0x41047a60L, 0xdf60efc3L, 0xa867df55L, 0x316e8eefL,
  0x4669be79L, 0xcb61b38cL, 0xbc66831aL, 0x256fd2a0L, 0x5268e236L,
  0xcc0c7795L, 0xbb0b4703L, 0x220216b9L, 0x5505262fL, 0xc5ba3bbeL,
  0xb2bd0b28L, 0x2bb45a92L, 0x5cb36a04L, 0xc2d7ffa7L, 0xb5d0cf31L,
  0x2cd99e8bL, 0x5bdeae1dL, 0x9b64c2b0L, 0xec63f226L, 0x756aa39cL,
  0x026d930aL, 0x9c0906a9L, 0xeb0e363fL, 0x72076785L, 0x05005713L,
  0x95bf4a82L, 0xe2b87a14L, 0x7bb12baeL, 0x0cb61b38L, 0x92d28e9bL,
  0xe5d5be0dL, 0x7cdcefb7L, 0x0bdbdf21L, 0x86d3d2d4L, 0xf1d4e242L,
  0x68ddb3f8L, 0x1fda836eL, 0x81be16cdL, 0xf6b9265bL, 0x6fb077e1L,
  0x18b74777L, 0x88085ae6L, 0xff0f6a70L, 0x66063bcaL, 0x11010b5cL,
  0x8f659effL, 0xf862ae69L, 0x616bffd3L, 0x166ccf45L, 0xa00ae278L,
  0xd70dd2eeL, 0x4e048354L, 0x3903b3c2L, 0xa7672661L, 0xd06016f7L,
  0x4969474dL, 0x3e6e77dbL, 0xaed16a4aL, 0xd9d65adcL, 0x40df0b66L,
  0x37d83bf0L, 0xa9bcae53L, 0xdebb9ec5L, 0x47b2cf7fL, 0x30b5ffe9L,
  0xbdbdf21cL, 0xcabac28aL, 0x53b39330L, 0x24b4a3a6L, 0xbad03605L,
  0xcdd70693L, 0x54de5729L, 0x23d967bfL, 0xb3667a2eL, 0xc4614ab8L,
  0x5d681b02L, 0x2a6f2b94L, 0xb40bbe37L, 0xc30c8ea1L, 0x5a05df1bL,
  0x2d02ef8dL
};

/* Check the content of the array.  */
static bool
match_crc_table (tree crc_table)
{
  const unsigned LOW_BOUND = 0;
  const unsigned UP_BOUND = 255;
  const unsigned ELEMENT_SIZE = 8;
  tree low_bound = array_ref_low_bound (crc_table);
  tree up_bound = array_ref_up_bound (crc_table);
  tree element_size = array_ref_element_size (crc_table);
  if (!tree_fits_uhwi_p(low_bound) || !tree_fits_uhwi_p(up_bound) ||
      !tree_fits_uhwi_p(element_size))
    return false;
  unsigned HOST_WIDE_INT lb = tree_to_uhwi (low_bound);
  unsigned HOST_WIDE_INT ub = tree_to_uhwi (up_bound);
  unsigned HOST_WIDE_INT es = tree_to_uhwi (element_size);
  if (lb != LOW_BOUND || ub != UP_BOUND || es != ELEMENT_SIZE)
    return false;

  tree decl = TREE_OPERAND (crc_table, 0);
  tree ctor = ctor_for_folding(decl);
  for (int i = lb; i <= ub; i++)
    {
      unsigned HOST_WIDE_INT val = tree_to_uhwi (CONSTRUCTOR_ELT (ctor,
                                                                  i)->value);
      if (crc_32_tab[i] != val)
        return false;
    }
  return true;
}

/* Check the crc table.  The loop should have only one data reference. 
   And match the data reference with the predefined array.  */
static bool
crc_table_check (class loop *loop)
{
  tree crc_table = NULL;
  if (!only_one_array_read (loop, crc_table))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\nTable check fail. not only single array "
                            "is read.\n");
      return false;
    }
  if (!match_crc_table (crc_table))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\nTable check fail.  Table not matching.\n");
      return false;
    }
  return  true;
}

/* Check whether the evolution pattern of phi is phi = SSA_NAME + target*/
static bool
evolution_pattern_plus_with_p (class loop *loop, gphi *phi,
                               unsigned HOST_WIDE_INT target)
{
  edge backedge = find_edge (loop->latch, loop->header);
  if (backedge == NULL)
    return false;
  tree evolution_node = PHI_ARG_DEF_FROM_EDGE (phi, backedge);
  gimple *evolution_expr = SSA_NAME_DEF_STMT (evolution_node);

  if (evolution_expr && (gimple_assign_rhs_code (evolution_expr) == PLUS_EXPR ||
                         gimple_assign_rhs_code (evolution_expr)
                             == POINTER_PLUS_EXPR))
    {
      tree rhs1 = gimple_assign_rhs1 (evolution_expr);
      tree rhs2 = gimple_assign_rhs2 (evolution_expr);
      if (TREE_CODE (rhs1) == SSA_NAME && TREE_CODE (rhs2) == INTEGER_CST
          && tree_to_uhwi (rhs2) == target)
        return true;
    }
  return false;
}

/* Check whether there are only 3 phi nodes in the header block.
   Return 3 phi nodes in the capture.  */
static bool
check_num_of_phi (basic_block header,  gphi *capture[])
{
  gphi *phi;
  gphi_iterator gsi;
  int num_of_phi = 0;

  for (gsi = gsi_start_phis (header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      phi = gsi.phi ();
      if (phi)
        num_of_phi++;
      if (num_of_phi > 3)
        return false;
      capture[num_of_phi - 1] = phi;
    }
  /* Phi node should be exactly 3.  */
  return num_of_phi == 3;   
}

/* Check the evolution pattern of three phi nodes.
   Should be one of the node +1 every time (s), one of the node -1
   every time (n), and a 3rd one neither (c).  Return 3 phi nodes in
   the capture with the order of s,n,c.*/
static bool
check_evolution_pattern (class loop *loop,  gphi *capture[])
{
  gphi *s = NULL;
  gphi *n = NULL;
  gphi *c = NULL;

  for (int i = 0; i < 3; i++)
    {
      if (evolution_pattern_plus_with_p (loop, capture[i], 1))
        {
          if (s != NULL)
            return false;
          s = capture[i];
          phi_s = s;
        }
      else if (evolution_pattern_plus_with_p (loop, capture[i], 4294967295))
        {
          if (n != NULL)
            return false;
          n = capture[i];
        }
      else
        {
          if (c != NULL)
            return false;
          c = capture[i];
          phi_c = c;
        }
    }

  // Some envolution pattern cannot find 
  if (!n || !s || !c)
    return false;

  capture[0] = s;
  capture[1] = n;
  capture[2] = c;
  return true;
}
/* Check the calculation pattern before and after the crc_table array read stmt.
   _7 = crc_32_tab[_6];
   The caculation of index _6 should be the result of a sequency of calculation
   by the s and c 
   The result of the array read _7 should be used to calculate the new c.  */
static bool
check_calculation_pattern (class loop *loop,  gphi *capture[])
{
  gphi *s = capture[0];
  gphi *c = capture[2];
  tree res_ops[3];
  tree index = TREE_OPERAND (gimple_assign_rhs1 (crc_table_read_stmt), 1);

  /* Try to match
  _4 = (int) _3; //NOP_EXPR (SSA_NAME @2)
  _5 =  _4 ^ c_10; //BIT_XOR_EXPR (SSA_NAME, PHI @1)
  _6 = _5 & 255; //BIT_XOR_EXPR (SSA_NAME, INTEGER_CST@3)
  */
  if (!gimple_crc_match_index (index, res_ops, NULL))
    return false;
  gimple *s_res_stmt = SSA_NAME_DEF_STMT (res_ops[0]);
  if (!s_res_stmt)
    return false;
  gimple *s_def_stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (s_res_stmt));
  if (!s_def_stmt)
    return false;
  tree s_res = TREE_OPERAND (gimple_assign_rhs1 (s_def_stmt), 0);
  if (res_ops[1] != gimple_phi_result (c) || s_res != gimple_phi_result (s))
    return false;

  /* Try to match 
  _8 = c_12 >> 8; // RSHIFT_EXPR (SSA_NAME @1, INTEGER_CST @2)
  c_19 = _7 ^ _8; // BIT_XOR_EXPR (SSA_NAME@3, SSA_NAME)
  */
  edge backedge = find_edge (loop->latch, loop->header);
  tree updated_c = PHI_ARG_DEF_FROM_EDGE (c, backedge);
  if (!gimple_crc_match_res (updated_c, res_ops, NULL))
    return false;
  if (res_ops[0] != gimple_phi_result (c)
      || res_ops[2] != gimple_assign_lhs (crc_table_read_stmt))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\n gimple_crc_match_res pattern check failed.\n");
      return false;
    }

  return true;
}

/* Check the exit condition is n != 0.  */
static bool
check_exit_condition (class loop *loop,  gphi *n)
{
  edge backedge = find_edge (loop->latch, loop->header);
  gimple *cond_stmt = gsi_stmt (gsi_last_bb (loop->header));
  if (!cond_stmt || gimple_code (cond_stmt) != GIMPLE_COND 
      || gimple_cond_code (cond_stmt) != NE_EXPR
      || gimple_cond_lhs (cond_stmt) != PHI_ARG_DEF_FROM_EDGE (n, backedge)
      || tree_to_uhwi (gimple_cond_rhs (cond_stmt)) != 0)
    return false;
  
  return  true;
}

/* Check the loop body. The loop body we are trying to match is

# s_10 = PHI <s_14(D)(6), s_18(7)>
# n_11 = PHI <n_17(D)(6), n_20(7)>
# c_12 = PHI <c_16(6), c_19(7)>
_1 = (int) c_12;
s_18 = s_10 + 1;
_3 = *s_10;
_4 = (int) _3;
_5 = _1 ^ _4;
_6 = _5 & 255;
_7 = crc_32_tab[_6];
_8 = c_12 >> 8;
c_19 = _7 ^ _8;
n_20 = n_11 + 4294967295;
if (n_20 != 0)
  goto <bb 7>; [INV]
else
  goto <bb 5>; [INV]

which is doing a very simple calculation
do {
        c = crc_32_tab[(c ^ (*s++)) & 0xff] ^ (c >> 8);
} while (--n);

In this case ,we don't want this loop to have any other operation inside.
so the matching condition is 
1. There are only 3 loop variant during each itoration, namely s,c,n,  
   which is limited by the condition that the loop have exactly 3 phi nodes.
2. The 3 loop variants should have evolution pattern as 1 of the 3 nodes is
   increased by 1 every itoration, 1 of the 3 nodes is decreased by 1 every itor
   and the 3rd one is neither. These three tree node SSA value will be captured
   for the later arithmatic pattern matching
3. Pattern matching for the index of crc_table
4. pattern matching for the result of c calcuation after read from crc_table
5. The exit condition matching. 
  */
static bool
crc_loop_body_check (class loop *loop)
{
  basic_block header = loop->header;
  gphi *capture[3];
  if (!check_num_of_phi(header, capture))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\n num of phi noeds check failed.\n");
      return false;
    }
  if (!check_evolution_pattern (loop, capture))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\n evolution pattern check failed.\n");
      return false;
    }
  if (!check_calculation_pattern (loop, capture))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\n calculation pattern check failed.\n");
      return false;
    }
  if (!check_exit_condition (loop, capture[1]))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\n exit condition check failed.\n");
      return false;
    }
  return true;
}

static bool check_prev_bb (basic_block prev_bb, enum tree_code code)
{
  gimple_stmt_iterator gsi;
  gimple *stmt;
  for (gsi = gsi_start_bb (prev_bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      if (stmt == NULL)
        return false;

      if (gimple_code (stmt) == GIMPLE_COND
          && gimple_cond_code (stmt) == code
          && TREE_CODE (gimple_cond_rhs (stmt)) == INTEGER_CST
          && tree_int_cst_sgn (gimple_cond_rhs (stmt)) == 0)
        return true;
    }
  return false;
}

/* Check the prev_bb of prev_bb of loop header.  The prev_bb we are trying to
match is

c_15 = crc;
if (n_16 (D) != 0)
  goto <bb 6>; [INV]
else
  goto <bb 5>; [INV]

  In this case , we must be sure that the n is not zero.
  so the match condition is
  1 the n is not zero.

  <bb 2> :
if (s_13 (D) == 0B)
  goto <bb 5>; [INV]
else
  goto <bb 3>; [INV]

  In this case, we must be sure the s is not NULL.
  so the match condition is
  1 the s is not NULL.
*/
static bool
crc_prev_bb_of_loop_header_check (class loop *loop)
{
  basic_block header = loop->header;
  basic_block prev_header_bb = header->prev_bb;
  if (NULL == prev_header_bb)
    return false;

  basic_block prev_prev_header_bb = prev_header_bb->prev_bb;
  if (NULL == prev_prev_header_bb)
    return false;

  if (!check_prev_bb (prev_prev_header_bb, NE_EXPR))
    return false;

  basic_block first_bb = prev_prev_header_bb->prev_bb;
  if (NULL == first_bb)
    return false;

  if (!check_prev_bb (first_bb, EQ_EXPR))
    return false;

  return true;
}

static bool
match_crc_loop (class loop *loop)
{
  if (!crc_loop_form_check (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\nWrong loop form for crc matching.\n");
      return false;
    }
  if (!crc_table_check (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\nWrong crc table for crc matching.\n");
      return false;
    }
  if (!crc_loop_body_check (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\nWrong loop body for crc matching.\n");
      return false;
    }
  if (!crc_prev_bb_of_loop_header_check (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\nWrong prev basic_blocks of loop header for"
                            " crc matching.\n");
      return false;
    }
    
  init_origin_loop_structure ();
  if (!get_origin_loop_info (loop))
    return false;

  return true;
}

static void
create_new_bb (basic_block &new_bb, basic_block after_bb,
               basic_block dominator_bb, class loop *outer)
{
  new_bb = create_empty_bb (after_bb);
  add_bb_to_loop (new_bb, outer);
  set_immediate_dominator (CDI_DOMINATORS, new_bb, dominator_bb);
}

static void
change_preheader_bb (edge entry_edge)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;
  tree lhs1;

  lhs1 = create_tmp_var (TREE_TYPE (origin_loop.base_n),"nn");
  lhs1 = make_ssa_name (lhs1);
  gsi = gsi_last_bb (entry_edge->src);
  g = gimple_build_assign (lhs1, RSHIFT_EXPR, origin_loop.base_n,
                           build_int_cst (TREE_TYPE (origin_loop.base_n), 2));
  gimple_seq_add_stmt (&stmts, g);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  nn_tree = lhs1;
  set_current_def (nn_tree, lhs1);
  nn_map.put (entry_edge->src, lhs1);
}

static gphi *
create_phi_node_for_bb (tree old_name, basic_block bb)
{
  gphi *phi = create_phi_node (NULL_TREE, bb);
  create_new_def_for (old_name, phi, gimple_phi_result_ptr (phi));
  return phi;
}

static gimple *
call_builtin_fun (int code,tree &lhs, tree arg1, tree arg2)
{
  unsigned int builtin_code = targetm.get_crc_builtin_code (code, true);
  // Get the decl of __builtin_aarch64_crc32w
  tree fn = targetm.builtin_decl (builtin_code, true);
  if (!fn || fn == error_mark_node)
    fatal_error (input_location,
                 "target specific builtin not available");
  gimple *call_builtin = gimple_build_call (fn, 2, arg1, arg2);
  lhs = make_ssa_name (unsigned_type_node);
  gimple_call_set_lhs (call_builtin, lhs);

  return call_builtin;
}

/* Create loop_header and loop_latch for new loop
   <bb 5> :
   # s_14 = PHI <s_23(D)(4), s_30(5)>
   # c_16 = PHI <c_25(4), c_29(5)>
   # nn_19 = PHI <nn_27(4), nn_31(5)>
   _1 = (unsigned int) c_16;
   _2 = MEM[(uint32_t *)s_14];
   _40 = __builtin_aarch64_crc32w (_1, _2);
   c_29 = (long unsigned int) _40;
   s_30 = s_14 + 4;
   nn_31 = nn_19 + 4294967295;
   if (nn_31 != 0)
   The IR of bb is as above.  */
static void
create_loop_bb (basic_block &loop_bb, basic_block after_bb,
                basic_block dominator_bb, class loop *outer, edge entry_edge)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;
  gphi *phi_s_loop;
  gphi *phi_c_loop;
  gphi *phi_nn_loop;

  create_new_bb (loop_bb, after_bb, dominator_bb, outer);
  redirect_edge_and_branch (entry_edge, loop_bb);
  gsi = gsi_last_bb (loop_bb);
  tree entry_nn = get_current_def (nn_tree);
  phi_s_loop = create_phi_node_for_bb (origin_loop.base_s, loop_bb);
  phi_c_loop = create_phi_node_for_bb (origin_loop.base_c, loop_bb);
  phi_nn_loop = create_phi_node_for_bb (entry_nn, loop_bb);

  tree res_s = gimple_phi_result (phi_s_loop);
  tree res_nn = gimple_phi_result (phi_nn_loop);
  tree lhs1 = gimple_build (&stmts, NOP_EXPR, unsigned_type_node,
                            gimple_phi_result (phi_c_loop));
  g = gimple_build_assign (make_ssa_name (unsigned_type_node),
                           fold_build2 (MEM_REF, unsigned_type_node, res_s,
                                        build_int_cst (
                                            build_pointer_type (
                                                unsigned_type_node),0)));
  gimple_seq_add_stmt (&stmts, g);
  tree lhs2 = gimple_assign_lhs (g);  // _2 = MEM[(uint32_t *)s_14];
  unsigned int code = AARCH64_BUILTIN_CRC32W;
  tree lhs3;
  gimple *build_crc32w = call_builtin_fun (code, lhs3, lhs1, lhs2);
  crc_map.put (loop_bb, lhs3);
  gimple_seq_add_stmt (&stmts, build_crc32w);

  tree lhs4 = copy_ssa_name (origin_loop.base_c);
  g = gimple_build_assign (lhs4, NOP_EXPR, lhs3);
  gimple_seq_add_stmt (&stmts, g);
  c_map.put (loop_bb, lhs4);

  tree lhs5 = copy_ssa_name (origin_loop.base_s);
  g = gimple_build_assign (lhs5, POINTER_PLUS_EXPR, res_s,
                           build_int_cst (sizetype, 4));
  gimple_seq_add_stmt (&stmts, g);
  s_map.put (loop_bb, lhs5);

  tree lhs6 = copy_ssa_name (nn_tree);
  g = gimple_build_assign (lhs6, PLUS_EXPR, res_nn,
                           build_int_cst (TREE_TYPE (res_nn), 4294967295));
  gimple_seq_add_stmt (&stmts,g);
  nn_map.put (loop_bb, lhs6);

  gcond *cond_stmt = gimple_build_cond (NE_EXPR, lhs6, origin_loop.limit,
                                        NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&stmts, cond_stmt);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
}

/*  <bb 6> :
    # c_6 = PHI <c_29(5)>
    # s_46 = PHI <s_30(5)>
    _44 = n_26(D) & 2;
    if (_44 != 0)
    The IR of bb is as above.  */
static void
create_cond_bb (basic_block &cond_bb, basic_block after_bb,
                basic_block dominator_bb, class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gphi *phi_s_loop;
  gphi *phi_c_loop;

  create_new_bb (cond_bb, after_bb, dominator_bb, outer);
  gsi = gsi_last_bb (cond_bb);
  tree entry_nn = get_current_def (nn_tree);
  phi_s_loop = create_phi_node_for_bb (origin_loop.base_s, cond_bb);
  phi_c_loop = create_phi_node_for_bb (origin_loop.base_c, cond_bb);
  tree res_s = gimple_phi_result (phi_s_loop);
  set_current_def (origin_loop.base_s, res_s);
  s_map.put (cond_bb, res_s);
  tree res_c = gimple_phi_result (phi_c_loop);
  set_current_def (origin_loop.base_c, res_c);
  c_map.put (cond_bb, res_c);

  tree lhs1 = gimple_build (&stmts, BIT_AND_EXPR,
                            TREE_TYPE (origin_loop.base_n), origin_loop.base_n,
                            build_int_cst (TREE_TYPE (origin_loop.base_n), 2));
  gcond *cond_stmt = gimple_build_cond (NE_EXPR, lhs1, origin_loop.limit,
                                        NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&stmts, cond_stmt);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
}

/*  <bb 7> :
    _7 = MEM[(uint16_t *)s_46];
    _41 = __builtin_aarch64_crc32h (_8, _7);
    c_33 = (long unsigned int) _41;
    s_34 = s_30 + 2;
    The IR of bb is as above.  */
static void
create_cond_true_bb (basic_block &cond_true_bb, basic_block after_bb,
                     basic_block dominator_bb, class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple *g;
  gimple_stmt_iterator gsi;

  create_new_bb (cond_true_bb, after_bb, dominator_bb, outer);
  gsi = gsi_last_bb (cond_true_bb);
  tree s_46 = *(s_map.get (after_bb));
  tree type = build_pointer_type (short_unsigned_type_node);
  g = gimple_build_assign (make_ssa_name (short_unsigned_type_node),
                           fold_build2 (MEM_REF, short_unsigned_type_node, s_46,
                                        build_int_cst (type, 0)));
  gimple_seq_add_stmt (&stmts,g);
  tree lhs1 = gimple_assign_lhs (g);  // _7 = MEM[(uint16_t *)s_46];
  unsigned int code = AARCH64_BUILTIN_CRC32H;
  tree lhs2;
  gimple *call_builtin = call_builtin_fun (code, lhs2,
                             *(crc_map.get (
                                  cond_true_bb->prev_bb->prev_bb)), lhs1);
  crc_map.put (cond_true_bb,lhs2);
  gimple_seq_add_stmt (&stmts, call_builtin);

  tree lhs3 = copy_ssa_name (origin_loop.base_c);
  g = gimple_build_assign (lhs3, NOP_EXPR, lhs2);
  gimple_seq_add_stmt (&stmts, g);
  c_map.put (cond_true_bb, lhs3);

  tree lhs5 = copy_ssa_name (s_46);
  g = gimple_build_assign (lhs5, POINTER_PLUS_EXPR, s_46,
                           build_int_cst (sizetype, 2)); //  s_30 + 2;
  gimple_seq_add_stmt (&stmts, g);
  s_map.put (cond_true_bb, lhs5);

  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  s_map.put (cond_true_bb, lhs5);
}

/* <bb 8> :
  # s_15 = PHI <s_46(6), s_34(7)>
  # c_17 = PHI <c_6(6), c_33(7)>
  _3 = n_26(D) & 1;
  if (_3 != 0)
   The IR of bb is as above.  */
static void
create_cond_false_bb (basic_block &cond_false_bb, basic_block after_bb,
                      basic_block dominator_bb, class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gphi *phi_s_cond_true_bb;
  gphi *phi_c_cond_true_bb;

  create_new_bb (cond_false_bb, after_bb, dominator_bb, outer);
  make_single_succ_edge (after_bb, cond_false_bb, EDGE_FALLTHRU);

  tree entry_s = get_current_def (origin_loop.base_s);
  phi_s_cond_true_bb = create_phi_node_for_bb (entry_s, cond_false_bb);
  tree entry_c = get_current_def (origin_loop.base_c);
  phi_c_cond_true_bb = create_phi_node_for_bb (entry_c, cond_false_bb);
  tree res_s = gimple_phi_result (phi_s_cond_true_bb);
  set_current_def (origin_loop.base_s, res_s);
  s_map.put (cond_false_bb, res_s);
  tree res_c = gimple_phi_result (phi_c_cond_true_bb);
  set_current_def (origin_loop.base_c, res_c);
  c_map.put (cond_false_bb, res_c);

  gsi = gsi_last_bb (cond_false_bb);
  tree lhs1 = gimple_build (&stmts, BIT_AND_EXPR,
                            TREE_TYPE (origin_loop.base_n), origin_loop.base_n,
                            build_int_cst (TREE_TYPE (origin_loop.base_n), 1));
  gcond *cond_stmt = gimple_build_cond (NE_EXPR, lhs1, origin_loop.limit,
                                        NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&stmts, cond_stmt);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
}

/*  <bb 9> :
  _11 = (unsigned int) c_17;
  _12 = *s_15;
  _42 = __builtin_aarch64_crc32b (_11, _12);
  c_36 = (long unsigned int) _42;
  The IR of bb is as above.  */
static void
create_lastcond_true_bb (basic_block &new_bb, basic_block after_bb,
                         basic_block dominator_bb, class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;

  create_new_bb (new_bb, after_bb, dominator_bb, outer);
  gsi = gsi_last_bb (new_bb);

  tree lhs1 = gimple_build (&stmts, NOP_EXPR, unsigned_type_node,
                            get_current_def (origin_loop.base_c));
  tree lhs2;
  tree s_15 = get_current_def (origin_loop.base_s);
  g = gimple_build_assign (make_ssa_name (unsigned_char_type_node),
                           fold_build2 (MEM_REF, unsigned_char_type_node, s_15,
                                        build_int_cst (TREE_TYPE (s_15), 0)));
  gimple_seq_add_stmt (&stmts, g);
  lhs2 = gimple_assign_lhs (g);

  unsigned int code = AARCH64_BUILTIN_CRC32B;
  tree lhs3;
  gimple *call_builtin = call_builtin_fun (code, lhs3, lhs1, lhs2);
  crc_map.put (new_bb,lhs3);
  gimple_seq_add_stmt (&stmts,call_builtin);

  tree lhs4 = copy_ssa_name (origin_loop.base_c);
  g = gimple_build_assign (lhs4, NOP_EXPR, lhs3);
  gimple_seq_add_stmt (&stmts, g);
  c_map.put (new_bb, lhs4);

  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
}

static bool
optional_add_phi_arg (gphi * phi, tree phi_res, tree phi_arg, edge e)
{
  location_t loc;
  if (same_ssa_name_var_p (phi_arg, phi_res))
    {
      if (virtual_operand_p (phi_arg))
        loc = UNKNOWN_LOCATION;
      else
        loc = gimple_location (SSA_NAME_DEF_STMT (phi_arg));
      add_phi_arg (phi, phi_arg, e, loc);

      return true;
    }

  return false;
}

/* Add phi_arg for bb with phi node.  */
static void
update_phi_nodes (basic_block bb)
{
  edge e;
  edge_iterator ei;
  gphi *phi;
  gphi_iterator gsi;
  tree res;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      phi = gsi.phi ();
      res = gimple_phi_result (phi);

      FOR_EACH_EDGE (e, ei, bb->preds)
      {
        if (PHI_ARG_DEF_FROM_EDGE (phi, e))
          continue;
        tree var_c;
        tree *ptr_var_c = c_map.get (e->src);
        if (ptr_var_c == NULL)
          var_c = origin_loop.base_c;
        else
          var_c = *ptr_var_c;
        if (optional_add_phi_arg (phi, res, var_c, e))
          continue;

        tree var_nn;
        tree *ptr_var_nn = nn_map.get (e->src);
        if (ptr_var_nn == NULL)
          var_nn = nn_tree;
        else
          var_nn = *ptr_var_nn;
        if (optional_add_phi_arg (phi, res, var_nn, e))
          continue;

        tree var_s;
        tree *ptr_var_s = s_map.get (e->src);
        if (ptr_var_s == NULL)
          var_s = origin_loop.base_s;
        else
          var_s = *ptr_var_s;
        if (optional_add_phi_arg (phi, res, var_s, e))
          continue;
      }
    }
}

static void
create_new_loops (edge entry_edge)
{
  class loop *new_loop = NULL;
  basic_block loop_bb, cond_bb, cond_true_bb, cond_false_bb, lastcond_true_bb;
  class loop *outer = entry_edge->src->loop_father;
  change_preheader_bb (entry_edge);

  create_loop_bb (loop_bb, entry_edge->src, entry_edge->src, outer, entry_edge);
  create_cond_bb (cond_bb, loop_bb, loop_bb, outer);
  make_edge (loop_bb, loop_bb, EDGE_TRUE_VALUE);
  make_edge (loop_bb, cond_bb, EDGE_FALSE_VALUE);
  update_phi_nodes (loop_bb);

  new_loop = alloc_loop ();
  new_loop->header = loop_bb;
  new_loop->latch = loop_bb;
  add_loop (new_loop, outer);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nPrint byte new loop %d:\n", new_loop->num);
      flow_loop_dump (new_loop, dump_file, NULL, 1);
      fprintf (dump_file, "\n\n");
    }

  create_cond_true_bb (cond_true_bb, cond_bb, cond_bb, outer);
  make_edge (cond_bb, cond_true_bb, EDGE_TRUE_VALUE);
  create_cond_false_bb (cond_false_bb, cond_true_bb, cond_bb, outer);
  make_edge (cond_bb, cond_false_bb, EDGE_FALSE_VALUE);
  update_phi_nodes (cond_bb);
  update_phi_nodes (cond_false_bb);
  create_lastcond_true_bb (lastcond_true_bb, cond_false_bb,
                           cond_false_bb, outer);
  make_edge (cond_false_bb, lastcond_true_bb, EDGE_TRUE_VALUE);
  make_edge (cond_false_bb, origin_loop.exit_bb, EDGE_FALSE_VALUE);
  make_single_succ_edge (lastcond_true_bb, origin_loop.exit_bb, EDGE_FALLTHRU);

  update_phi_nodes (origin_loop.exit_bb);
  remove_edge (origin_loop.exit_edge);
}

/* Clear information about the original loop.  */
static void
remove_origin_loop (class loop *loop)
{
  basic_block *body = get_loop_body_in_dom_order (loop);
  unsigned n = loop->num_nodes;
  for (int i = 0; i < n; ++i)
    delete_basic_block (body[i]);
  free (body);
  delete_loop (loop);
}

/* Make sure that the dominance relationship of the newly inserted cfg
   is not missing.  */
static void
update_loop_dominator (cdi_direction dir)
{
  gcc_assert (dom_info_available_p (dir));

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
  {
    basic_block imm_bb = get_immediate_dominator (dir, bb);
    if (!imm_bb || bb == origin_loop.exit_bb)
      {
        set_immediate_dominator (CDI_DOMINATORS, bb,
                               recompute_dominator (CDI_DOMINATORS, bb));
        continue;
      }
  }
}

/* Perform the conversion of origin_loop to new_loop.  */
static void
convert_to_new_loop (class loop *loop)
{
  create_new_loops (origin_loop.entry_edge);
  remove_origin_loop (loop);
  update_loop_dominator (CDI_DOMINATORS);
  update_ssa (TODO_update_ssa);
}

/* The main entry of loop crc optimizes.  */
static unsigned int
tree_ssa_loop_crc ()
{
#ifndef TARGET_CRC32
  return 0;
#else
  if (TARGET_CRC32 == false)
    {
      warning (OPT____,"The loop-crc optimization is not working." \
                      "You should make sure that the specified architecture" \
                      "supports crc:-march=armv8.1-a");
      return 0;
    }
  unsigned int todo = 0;
  class loop *loop;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      flow_loops_dump (dump_file, NULL, 1);
      fprintf (dump_file, "\nStarting the loop_crc pass\n");
    }

  enum li_flags LI = LI_FROM_INNERMOST;
  for (auto loop : loops_list (cfun, LI))
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      {
        fprintf (dump_file, "======================================\n");
        fprintf (dump_file, "Processing loop %d:\n", loop->num);
        fprintf (dump_file, "======================================\n");
        flow_loop_dump (loop, dump_file, NULL, 1);
        fprintf (dump_file, "\n\n");
      }

    if (match_crc_loop (loop))
      {
        if (dump_file && (dump_flags & TDF_DETAILS))
          {
             fprintf (dump_file, "The %dth loop form is success matched,"
                                "and the loop can be optimized.\n",
                     loop->num);
          }

        convert_to_new_loop (loop);
        todo |= (TODO_update_ssa);
      }
  }
  return todo;
#endif
}

/* Loop crc.  */

namespace {
const pass_data pass_data_tree_loop_crc =
{
  GIMPLE_PASS,
  "loop_crc",
  OPTGROUP_LOOP,
  TV_TREE_LOOP_CRC,
  (PROP_cfg | PROP_ssa),
  0,
  0,
  0,
  (TODO_update_ssa | TODO_verify_all)
};

class pass_loop_crc : public gimple_opt_pass
{
public:
  pass_loop_crc (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_loop_crc, ctxt)
  {}

  /* Opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *);
}; // Class pass_loop_crc

bool
pass_loop_crc::gate (function *)
{
  return (flag_loop_crc > 0 && optimize >= 3);
}

unsigned int
pass_loop_crc::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  /* Only supports LP64 data mode.  */
  if (TYPE_PRECISION (long_integer_type_node) != 64
      || POINTER_SIZE != 64 || TYPE_PRECISION (integer_type_node) != 32)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "The current data mode is not supported,"
                            "only the LP64 date mode is supported.\n");
      return 0;
    }

  return tree_ssa_loop_crc ();
}

} // Anon namespace

gimple_opt_pass *
make_pass_loop_crc (gcc::context *ctxt)
{
  return new pass_loop_crc (ctxt);
}
