/* Array widen compare.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.

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

/* This pass handles scenarios similar to the following:

   uint32_t
   func (uint32_t len0, uint32_t len1, const uint32_t len_limit,
	 const uint8_t *const pb, const uint8_t *const cur)
   {
     uint32_t len = my_min (len0, len1);
     while (++len != len_limit)
       if (pb[len] != cur[len])
	 break;
     return len;
   }

   Features of this type of loop:
     1) the loop has two exits;
     2) One of the exits comes from the comparison result of the array;

   From the source code point of view, the pass completes the conversion of the
   above scenario into:

   uint32_t
   func (uint32_t len0, uint32_t len1, const uint32_t len_limit,
	 const uint8_t *const pb, const uint8_t *const cur)
   {
     uint32_t len = my_min (len0, len1);
     // align_loop
     for(++len; len + sizeof(uint64_t) <= len_limit; len += sizeof (uint64_t))
     {
       uint64_t a = *((uint64_t*)(cur+len));
       uint64_t b = *((uint64_t*)(pb+len));
       if (a != b)
       {
	 int lz = __builtin_ctzll (a ^ b);
	 len += lz / 8;
	 return len;
       }
     }
     // epilogue_loop
     for (;len != len_limit; ++len)
       if (pb[len] != cur[len])
	 break;
     return len;
   }

   This pass is to complete the conversion of such scenarios from the internal
   perspective of the compiler:
     1) determine_loop_form: The function completes the screening of such
			     scenarios;
     2) convert_to_new_loop: The function completes the conversion of
     			     origin_loop to new loops, and removes origin_loop;
     3) origin_loop_info: The structure is used to record important information
     			  of origin_loop: such as loop exit, growth step size
			  of loop induction variable, initial value
			  of induction variable, etc;
     4) create_new_loops: The function is used as the key content of the pass
			  to complete the creation of new loops.  */

/* The useful information of origin loop.  */

struct origin_loop_info
{
  tree base;		/* The initial index of the array in the old loop.  */
  tree limit;		/* The limit index of the array in the old loop.  */
  tree arr1;		/* Array 1 in the old loop.  */
  tree arr2;		/* Array 2 in the old loop.  */
  edge entry_edge;	/* The edge into the old loop.  */
  basic_block exit_bb1;
  basic_block exit_bb2;
  edge exit_e1;
  edge exit_e2;
  gimple *cond_stmt1;
  gimple *cond_stmt2;
  gimple *update_stmt;
  bool exist_prolog_assgin;
			/* Whether the marker has an initial value assigned
			   to the array index.  */
  unsigned HOST_WIDE_INT step;
  			/* The growth step of the loop induction variable.  */
};

typedef struct origin_loop_info origin_loop_info;

static origin_loop_info origin_loop;
hash_map <basic_block, tree> defs_map;

/* Dump the bb information in a loop.  */

static void
dump_loop_bb (struct loop *loop)
{
  basic_block *body = get_loop_body_in_dom_order (loop);
  basic_block bb = NULL;

  for (unsigned i = 0; i < loop->num_nodes; i++)
    {
      bb = body[i];
      if (bb->loop_father != loop)
	{
	  continue;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "===== the %dth bb of loop ==========:\n", i);
	  gimple_dump_bb (dump_file, bb, 0, dump_flags);
	  fprintf (dump_file, "\n");
	}
    }
  free (body);
}

/* Return true if the loop has precisely one backedge.  */

static bool
loop_single_backedge_p (class loop *loop)
{
  basic_block latch = loop->latch;
  if (!single_succ_p (latch))
    return false;

  edge e = single_succ_edge (latch);
  edge backedge = find_edge (latch, loop->header);

  if (e != backedge)
    return false;

  return true;
}

/* Return true if the loop has precisely one preheader BB.  */

static bool
loop_single_preheader_bb (class loop *loop)
{
  basic_block header = loop->header;
  if (EDGE_COUNT (header->preds) != 2)
    return false;

  edge e1 = EDGE_PRED (header, 0);
  edge e2 = EDGE_PRED (header, 1);

  if ((e1->src == loop->latch && e2->src->loop_father != loop)
      || (e2->src == loop->latch && e1->src->loop_father != loop))
    return true;

  return false;
}

/* Initialize the origin_loop structure.  */
static void
init_origin_loop_structure ()
{
  origin_loop.base = NULL;
  origin_loop.limit = NULL;
  origin_loop.arr1 = NULL;
  origin_loop.arr2 = NULL;
  origin_loop.exit_e1 = NULL;
  origin_loop.exit_e2 = NULL;
  origin_loop.exit_bb1 = NULL;
  origin_loop.exit_bb2 =NULL;
  origin_loop.entry_edge = NULL;
  origin_loop.cond_stmt1 = NULL;
  origin_loop.cond_stmt2 = NULL;
  origin_loop.update_stmt = NULL;
  origin_loop.exist_prolog_assgin = false;
  origin_loop.step = 0;
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

  if (!e)
    {
      gcc_assert (!loop_outer (loop));
      return single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
    }

  return e;
}

/* Make sure the exit condition stmt satisfies a specific form.  */

static bool
check_cond_stmt (gimple *stmt)
{
  if (!stmt)
    return false;
  if (gimple_code (stmt) != GIMPLE_COND)
    return false;

  if (gimple_cond_code (stmt) != NE_EXPR && gimple_cond_code (stmt) != EQ_EXPR)
    return false;

  tree lhs = gimple_cond_lhs (stmt);
  tree rhs = gimple_cond_rhs (stmt);

  /* The parameter that does not support the cond statement is not SSA_NAME.
     eg: if (len_1 != 100).  */
  if (TREE_CODE (lhs) != SSA_NAME || TREE_CODE (rhs) != SSA_NAME)
    return false;

  return true;
}

/* Record the exit information in the original loop including exit edge,
   exit bb block, exit condition stmt,
   eg: exit_eX origin_exit_bbX cond_stmtX.  */

static bool
record_origin_loop_exit_info (class loop *loop)
{
  bool found = false;
  edge e = NULL;
  unsigned i = 0;
  gimple *stmt;

  if (origin_loop.exit_e1 != NULL || origin_loop.exit_bb1 != NULL
      || origin_loop.exit_e2 != NULL || origin_loop.exit_bb2 != NULL
      || origin_loop.cond_stmt1 != NULL || origin_loop.cond_stmt2 != NULL)
    return false;

  auto_vec<edge> exit_edges = get_loop_exit_edges (loop);
  if (exit_edges == vNULL)
    return false;

  if (exit_edges.length () != 2)
    return false;

  FOR_EACH_VEC_ELT (exit_edges, i, e)
    {
      if (e->src == loop->header)
	{
	  origin_loop.exit_e1 = e;
	  origin_loop.exit_bb1 = e->dest;
	  stmt = gsi_stmt (gsi_last_bb (e->src));
	  if (check_cond_stmt (stmt))
	    origin_loop.cond_stmt1 = stmt;
	}
      else
	{
	  origin_loop.exit_e2 = e;
	  origin_loop.exit_bb2 = e->dest;
	  stmt = gsi_stmt (gsi_last_bb (e->src));
	  if (check_cond_stmt (stmt))
	    origin_loop.cond_stmt2 = stmt;
	}
    }

  if (origin_loop.exit_e1 != NULL && origin_loop.exit_bb1 != NULL
      && origin_loop.exit_e2 != NULL && origin_loop.exit_bb2 != NULL
      && origin_loop.cond_stmt1 != NULL && origin_loop.cond_stmt2 != NULL)
    found = true;

  return found;
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
  if (origin_loop.limit != NULL)
    return false;

  tree lhs = gimple_cond_lhs (stmt);
  tree rhs = gimple_cond_rhs (stmt);

  if (TREE_CODE (TREE_TYPE (lhs)) != INTEGER_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) != INTEGER_TYPE)
    return false;

  gimple *g = SSA_NAME_DEF_STMT (rhs);

  /* TODO: Currently, the input restrictions on lhs and rhs are implemented
     through PARM_DECL. We may consider releasing the restrictions later, and
     we need to consider the overall adaptation scenario and adding test
     cases. */
  if (ssa_name_var_p (rhs) && TREE_CODE (SSA_NAME_VAR (rhs)) == PARM_DECL
      && g && gimple_code (g) == GIMPLE_NOP
      && (ssa_name_var_p (lhs) && TREE_CODE (SSA_NAME_VAR (lhs)) != PARM_DECL))
    {
      origin_loop.limit = rhs;
    }
  else
    return false;

  if (origin_loop.limit != NULL)
    return true;

  return false;
}

/* Returns true only when the expression on the rhs code of stmt is PLUS_EXPR,
   rhs1 is SSA_NAME with the same var as origin_loop base, and rhs2 is
   INTEGER_CST.  */

static bool
check_update_stmt (gimple *stmt)
{
  if (!stmt)
    return false;

  if (gimple_assign_rhs_code (stmt) == PLUS_EXPR)
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      if (TREE_CODE (rhs1) == SSA_NAME && TREE_CODE (rhs2) == INTEGER_CST
	  && same_ssa_name_var_p (rhs1, origin_loop.base))
	{
	  origin_loop.step = tree_to_uhwi (rhs2);
	  if (origin_loop.step == 1)
	    return true;
	}
    }
  return false;
}

/* Get origin loop induction variable initial value.  */

static bool
get_iv_base (gimple *stmt)
{
  tree lhs = gimple_cond_lhs (stmt);
  if (origin_loop.base != NULL || origin_loop.update_stmt != NULL)
    return false;

  basic_block header = gimple_bb (stmt);

  gphi_iterator gsi;
  edge e;
  edge_iterator ei;
  tree iv_after;

  for (gsi = gsi_start_phis (header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree res = gimple_phi_result (phi);
      if (!same_ssa_name_var_p (res, lhs))
	continue;
      tree base = PHI_ARG_DEF_FROM_EDGE (phi, origin_loop.entry_edge);
      if (!same_ssa_name_var_p (base, lhs))
	return false;
      origin_loop.base = base;
      FOR_EACH_EDGE (e, ei, header->preds)
	{
	  if (e != origin_loop.entry_edge)
	    {
	      iv_after = PHI_ARG_DEF_FROM_EDGE (phi, e);
	      gimple *update = SSA_NAME_DEF_STMT (iv_after);
	      if (!check_update_stmt (update))
	        return false;
	      origin_loop.update_stmt = update;
	      if (gimple_bb (update) == header && iv_after == lhs)
		origin_loop.exist_prolog_assgin = true;
	    }
	}
    }

  if (origin_loop.base != NULL && origin_loop.update_stmt != NULL)
    return true;

  return false;
}

/* Record the upper bound and initial value of the induction variable in the
   original loop; When prolog_assign is present, make sure loop header is in
   simple form; And the interpretation of prolog_assign is as follows:
   eg: while (++len != limit)
	......
   For such a loop, ++len will be processed before entering header_bb, and the
   assign is regarded as the prolog_assign of the loop.  */

static bool
record_origin_loop_header (class loop *loop)
{
  basic_block header = loop->header;

  if (origin_loop.entry_edge != NULL || origin_loop.base != NULL
      || origin_loop.update_stmt != NULL || origin_loop.limit != NULL)
    return false;
  origin_loop.entry_edge = get_loop_preheader_edge (loop);

  gimple_stmt_iterator gsi;
  gimple *stmt;

  for (gsi = gsi_last_bb (header); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      stmt = gsi_stmt (gsi);
      if (stmt && is_gimple_debug (stmt))
        continue;
      if (stmt && gimple_code (stmt) == GIMPLE_COND)
	{
	  if (!get_iv_upper_bound (stmt))
	    return false;
	  if (!get_iv_base (stmt))
	    return false;
	}
      else if (stmt && gimple_code (stmt) == GIMPLE_ASSIGN)
	{
	  if (stmt != origin_loop.update_stmt || !origin_loop.exist_prolog_assgin)
	    return false;
	}
      else
	return false;
    }

  if (origin_loop.entry_edge != NULL && origin_loop.base != NULL
      && origin_loop.update_stmt != NULL && origin_loop.limit != NULL)
    return true;

  return false;
}

/* When prolog_assign does not exist, make sure that update_stmt exists in the
   loop latch, and its form is a specific form, eg:
   len_2 = len_1 + 1.  */

static bool
record_origin_loop_latch (class loop *loop)
{
  basic_block latch = loop->latch;
  gimple_stmt_iterator gsi;
  gimple *stmt;

  gsi = gsi_start_bb (latch);

  if (origin_loop.exist_prolog_assgin)
    {
      if (gsi_end_p (gsi))
	return true;
    }
  else
    {
      if (gsi_one_before_end_p (gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (stmt == origin_loop.update_stmt)
	    return true;
	}
    }
  return false;
}

/* Returns true when the DEF_STMT corresponding to arg0 of the mem_ref tree
   satisfies the POINTER_PLUS_EXPR type.  */

static bool
check_body_mem_ref (tree mem_ref)
{
  tree arg0 = TREE_OPERAND (mem_ref , 0);
  tree arg1 = TREE_OPERAND (mem_ref , 1);

  if (TREE_CODE (TREE_TYPE (arg0)) == POINTER_TYPE
      && TREE_CODE (arg1) == INTEGER_CST
      && tree_to_uhwi (arg1) == 0)
    {
      gimple *tmp_g = SSA_NAME_DEF_STMT (arg0);
      if (tmp_g && gimple_assign_rhs_code (tmp_g) == POINTER_PLUS_EXPR)
	return true;
    }
  return false;
}

/* Returns true if the rh2 of the current stmt comes from the base in the
   original loop.  */

static bool
check_body_pointer_plus (gimple *stmt, tree &tmp_index)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  if (TREE_CODE (TREE_TYPE (rhs1)) == POINTER_TYPE)
    {
      gimple *g = SSA_NAME_DEF_STMT (rhs2);
      if (g && gimple_assign_rhs_code (g) == NOP_EXPR)
	{
	  tree nop_rhs = gimple_assign_rhs1 (g);
	  if (same_ssa_name_var_p (nop_rhs, origin_loop.base))
	    {
	      if (!origin_loop.arr1)
		{
		  origin_loop.arr1 = rhs1;
		  tmp_index = rhs2;
		}
	      else if (!origin_loop.arr2)
		{
		  origin_loop.arr2 = rhs1;
		  if (tmp_index != rhs2)
		    return false;
		}
	      else
		return false;
	      return true;
	    }
	}
    }
  return false;
}

/* Record the array comparison information in the original loop, while ensuring
   that there are only statements related to cont_stmt in the loop body.  */

static bool
record_origin_loop_body (class loop *loop)
{
  basic_block body = gimple_bb (origin_loop.cond_stmt2);

  if (origin_loop.arr1 != NULL || origin_loop.arr2 != NULL)
    return false;

  gimple_stmt_iterator gsi;
  for (gsi = gsi_start_bb (body); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple_set_visited (gsi_stmt (gsi), false);
    }

  tree cond_lhs = gimple_cond_lhs (origin_loop.cond_stmt2);
  tree cond_rhs = gimple_cond_rhs (origin_loop.cond_stmt2);
  if (TREE_CODE (TREE_TYPE (cond_lhs)) != INTEGER_TYPE
      || TREE_CODE (TREE_TYPE (cond_rhs)) != INTEGER_TYPE)
    return false;

  auto_vec<tree> stack;
  tree tmp_index = NULL;
  stack.safe_push (cond_lhs);
  stack.safe_push (cond_rhs);
  gimple_set_visited (origin_loop.cond_stmt2, true);

  while (!stack.is_empty ())
    {
      tree op = stack.pop ();
      gimple *g = SSA_NAME_DEF_STMT (op);
      if (!g || gimple_bb (g) != body || !is_gimple_assign (g))
	continue;
      gimple_set_visited (g, true);
      if (gimple_assign_rhs_code (g) == MEM_REF)
	{
	  tree mem_ref = gimple_assign_rhs1 (g);
	  if (!check_body_mem_ref (mem_ref))
	    return false;
	  stack.safe_push (TREE_OPERAND (mem_ref , 0));
	}
      else if (gimple_assign_rhs_code (g) == POINTER_PLUS_EXPR)
	{
	  tree rhs2 = gimple_assign_rhs2 (g);
	  if (!check_body_pointer_plus (g, tmp_index))
	    return false;
	  stack.safe_push (rhs2);
	}
      else if (gimple_assign_rhs_code (g) == NOP_EXPR)
	{
	  tree rhs = gimple_assign_rhs1 (g);
	  if (!same_ssa_name_var_p (rhs, origin_loop.base))
	    return false;
	  stack.safe_push (rhs);
	}
      else
	return false;
    }
  bool allvisited = true;
  for (gsi = gsi_start_bb (body); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      if (!gimple_visited_p (gsi_stmt (gsi))
	  && !is_gimple_debug (gsi_stmt (gsi)))
	allvisited = false;
    }
  if (allvisited)
    {
      if (origin_loop.arr1 != NULL && origin_loop.arr2 != NULL)
	return true;
    }
  return false;
}

/* Dump the original loop information to see if the origin loop
   form matches.  */

static void
dump_origin_loop_info ()
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nThe origin loop info:\n");
      fprintf (dump_file, "\n    the origin_loop.limit is:\n");
      print_node (dump_file, "", origin_loop.limit, 0);
      fprintf (dump_file, "\n");
      fprintf (dump_file, "\n    the origin_loop.base is:\n");
      print_node (dump_file, "", origin_loop.base, 0);
      fprintf (dump_file, "\n");
      fprintf (dump_file, "\n    the origin_loop.arr1 is:\n");
      print_node (dump_file, "", origin_loop.arr1, 0);
      fprintf (dump_file, "\n");
      fprintf (dump_file, "\n    the origin_loop.arr2 is:\n");
      print_node (dump_file, "", origin_loop.arr2, 0);
      fprintf (dump_file, "\n");
      fprintf (dump_file, "\n    the origin_loop.cond_stmt1 is:\n");
      print_gimple_stmt (dump_file, origin_loop.cond_stmt1, 0);
      fprintf (dump_file, "\n");
      fprintf (dump_file, "\n    the origin_loop.cond_stmt2 is:\n");
      print_gimple_stmt (dump_file, origin_loop.cond_stmt2, 0);
      fprintf (dump_file, "\n");
      fprintf (dump_file, "\n    the origin_loop.update_stmt is:\n");
      print_gimple_stmt (dump_file, origin_loop.update_stmt, 0);
      fprintf (dump_file, "\n");
    }
}

/* Returns true only if the exit bb of the original loop is unique and its phi
   node parameter comes from the same variable.  */

static bool
check_exit_bb (class loop *loop)
{
  if (origin_loop.exit_bb1 != origin_loop.exit_bb2
      || flow_bb_inside_loop_p (loop, origin_loop.exit_bb1))
    return false;

  gphi_iterator gsi;
  for (gsi = gsi_start_phis (origin_loop.exit_bb1); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree res = gimple_phi_result (phi);
      if (!same_ssa_name_var_p (res, origin_loop.base))
	continue;
      if (gimple_phi_num_args (phi) == 2)
	{
	  tree arg0 = gimple_phi_arg_def (phi, 0);
	  tree arg1 = gimple_phi_arg_def (phi, 1);
	  if (arg0 == arg1)
	    return true;
	}
    }
  return false;
}

/* Make sure that the recorded origin_loop information meets the
   relative requirements.  */

static bool
check_origin_loop_info (class loop *loop)
{
  dump_origin_loop_info ();
  tree arr1_elem_size, arr2_elem_size;

  if (!check_exit_bb (loop))
    return false;

  if (TREE_CODE (origin_loop.base) != SSA_NAME)
    return false;

  if (!TYPE_READONLY (TREE_TYPE (origin_loop.limit)))
    return false;

  if (!TYPE_READONLY (TREE_TYPE (TREE_TYPE (origin_loop.arr1))))
    return false;

  if (!TYPE_READONLY (TREE_TYPE (TREE_TYPE (origin_loop.arr2))))
    return false;

  if (TREE_CODE (TREE_TYPE (origin_loop.arr1)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (origin_loop.arr2)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (TREE_TYPE (origin_loop.arr1))) != INTEGER_TYPE
      || TREE_CODE (TREE_TYPE (TREE_TYPE (origin_loop.arr2))) != INTEGER_TYPE)
    return false;

  arr1_elem_size = TYPE_SIZE (TREE_TYPE (TREE_TYPE (origin_loop.arr1)));
  arr2_elem_size = TYPE_SIZE (TREE_TYPE (TREE_TYPE (origin_loop.arr2)));

  if (tree_to_uhwi (arr1_elem_size) != 8 || tree_to_uhwi (arr2_elem_size) != 8)
    return false;

  return true;
}

/* Record the useful information of the original loop and judge whether the
   information meets the specified conditions.  */

static bool
check_record_loop_form (class loop *loop)
{
  if (!record_origin_loop_exit_info (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nFailed to record loop exit information.\n");
	}
      return false;
    }

  if (!record_origin_loop_header (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nFailed to record loop header information.\n");
	}
      return false;
    }

  if (!record_origin_loop_latch (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nFailed to record loop latch information.\n");
	}
      return false;
    }

  if (!record_origin_loop_body (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nFailed to record loop body information.\n");
	}
      return false;
    }

  if (!check_origin_loop_info (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nFailed to check origin loop information.\n");
	}
      return false;
    }

  return true;
}

/* The main entry for judging whether the loop meets some conditions.  */

static bool
determine_loop_form (class loop *loop)
{
  /* Currently only standard loops are processed, that is, only loop_header,
     loop_latch, loop_body 3 bb blocks are included.  */
  if (loop->inner || loop->num_nodes != 3)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nWrong loop form, there is inner loop or"
			      "redundant bb.\n");
	}
      return false;
    }

  if (single_exit (loop) || !loop->latch)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nWrong loop form, only one exit or loop_latch"
			      "does not exist.\n");
	}
      return false;
    }

  /* Support loop with only one backedge.  */
  if (!loop_single_backedge_p (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nWrong loop form, loop back edges are not"
			      "unique.\n");
	}
      return false;
    }

  /* Support loop with only one preheader BB.  */
  if (!loop_single_preheader_bb (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nWrong loop form, loop preheader bb are not"
			      "unique.\n");
	}
      return false;
    }

  init_origin_loop_structure ();
  if (!check_record_loop_form (loop))
    return false;

  return true;
}

/* Create prolog bb for newly constructed loop; When prolog_assign exists in
   the original loop, the corresponding assign needs to be added to prolog_bb;
   eg: <bb 7>
       len_16 = len_10 + 1
   Create simple copy statement when prolog_assign does not exist;
   eg: <bb 7>
       len_16 = len_10

   The IR of bb is as above.  */

static void
create_prolog_bb (basic_block &prolog_bb, basic_block after_bb,
		  basic_block dominator_bb, class loop *outer, edge entry_edge)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;
  tree lhs1;

  prolog_bb = create_empty_bb (after_bb);
  add_bb_to_loop (prolog_bb, outer);
  redirect_edge_and_branch (entry_edge, prolog_bb);
  set_immediate_dominator (CDI_DOMINATORS, prolog_bb, dominator_bb);
  gsi = gsi_last_bb (prolog_bb);
  lhs1 = copy_ssa_name (origin_loop.base);

  if (origin_loop.exist_prolog_assgin)
    g = gimple_build_assign (lhs1, PLUS_EXPR, origin_loop.base,
	  build_int_cst (TREE_TYPE (origin_loop.base), origin_loop.step));
  else
    g = gimple_build_assign (lhs1, NOP_EXPR, origin_loop.base);
  gimple_seq_add_stmt (&stmts, g);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  set_current_def (origin_loop.base, lhs1);
  defs_map.put (prolog_bb, lhs1);
}

/* Create preheader bb for new loop; In order to ensure the standard form of
   the loop, add a preheader_bb before loop_header.  */

static void
create_loop_pred_bb (basic_block &loop_pred_bb, basic_block after_bb,
		     basic_block dominator_bb, class loop *outer)
{
  loop_pred_bb = create_empty_bb (after_bb);
  add_bb_to_loop (loop_pred_bb, outer);
  set_immediate_dominator (CDI_DOMINATORS, loop_pred_bb, dominator_bb);
  defs_map.put (loop_pred_bb, get_current_def (origin_loop.base));
}

/* Add phi_arg for bb with phi node.  */

static void
rewrite_add_phi_arg (basic_block bb)
{
  edge e;
  edge_iterator ei;
  gphi *phi;
  gphi_iterator gsi;
  tree res;
  location_t loc;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      phi = gsi.phi ();
      res = gimple_phi_result (phi);

      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (PHI_ARG_DEF_FROM_EDGE (phi, e))
	    continue;
	  tree var = *(defs_map.get (e->src));
	  if (!same_ssa_name_var_p (var, res))
	    continue;
	  if (virtual_operand_p (var))
	    loc = UNKNOWN_LOCATION;
	  else
	    loc = gimple_location (SSA_NAME_DEF_STMT (var));
	  add_phi_arg (phi, var, e, loc);
	}
    }
}

/* Create loop_header BB for align_loop.
   eg: <bb 9>
       _18 = (long unsigned int) len_17;
       _19 = _18 + 8;
       _20 = (long unsigned int) len_limit_12 (D);
       if (_19 <= _20)

   The IR of bb is as above.  */

static void
create_align_loop_header (basic_block &align_loop_header, basic_block after_bb,
			  basic_block dominator_bb, class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gcond *cond_stmt;
  gphi *phi;
  tree res;

  tree entry_node = get_current_def (origin_loop.base);
  align_loop_header = create_empty_bb (after_bb);
  add_bb_to_loop (align_loop_header, outer);
  make_single_succ_edge (after_bb, align_loop_header, EDGE_FALLTHRU);
  set_immediate_dominator (CDI_DOMINATORS, align_loop_header, dominator_bb);
  gsi = gsi_last_bb (align_loop_header);
  phi = create_phi_node (NULL_TREE, align_loop_header);
  create_new_def_for (entry_node, phi, gimple_phi_result_ptr (phi));
  res = gimple_phi_result (phi);

  tree lhs1 = gimple_build (&stmts, NOP_EXPR, long_unsigned_type_node, res);
  tree lhs2 = gimple_build (&stmts, PLUS_EXPR, TREE_TYPE (lhs1), lhs1,
			    build_int_cst (TREE_TYPE (lhs1), 8));
  tree lhs3 = gimple_build (&stmts, NOP_EXPR, long_unsigned_type_node,
  				origin_loop.limit);
  cond_stmt = gimple_build_cond (LE_EXPR, lhs2, lhs3, NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&stmts, cond_stmt);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);

  set_current_def (origin_loop.base, res);
  defs_map.put (align_loop_header, res);
}

/* Create loop body BB for align_loop.
   eg: <bb 10>
       _21 = (sizetype) len_17;
       _22 = cur_15 (D) + _21;
       _23 = MEM[(long unsigned int *)_22];
       _24 = pb_13 (D) + _21;
       _25 = MEM[(long unsigned int *)_24];
       if (_23 != _25)

   The IR of bb is as above.  */

static void
create_align_loop_body_bb (basic_block &align_loop_body_bb,
			   basic_block after_bb, basic_block dominator_bb,
			   class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;
  gcond *cond_stmt;
  tree lhs1, lhs2;

  align_loop_body_bb = create_empty_bb (after_bb);
  add_bb_to_loop (align_loop_body_bb, outer);
  make_edge (after_bb, align_loop_body_bb, EDGE_TRUE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, align_loop_body_bb, dominator_bb);
  gsi = gsi_last_bb (align_loop_body_bb);

  tree var = gimple_build (&stmts, NOP_EXPR, sizetype,
			   get_current_def (origin_loop.base));
  lhs1 = gimple_build (&stmts, POINTER_PLUS_EXPR, TREE_TYPE (origin_loop.arr2),
  		       origin_loop.arr2, var);
  g = gimple_build_assign (make_ssa_name (long_unsigned_type_node),
	fold_build2 (MEM_REF, long_unsigned_type_node, lhs1,
	  build_int_cst (build_pointer_type (long_unsigned_type_node), 0)));
  gimple_seq_add_stmt (&stmts, g);
  lhs1 = gimple_assign_lhs (g);
  lhs2 = gimple_build (&stmts, POINTER_PLUS_EXPR, TREE_TYPE (origin_loop.arr1),
  		       origin_loop.arr1, var);
  g = gimple_build_assign (make_ssa_name (long_unsigned_type_node),
	fold_build2 (MEM_REF, long_unsigned_type_node, lhs2,
	  build_int_cst (build_pointer_type (long_unsigned_type_node), 0)));
  gimple_seq_add_stmt (&stmts, g);
  lhs2 = gimple_assign_lhs (g);
  cond_stmt = gimple_build_cond (gimple_cond_code (origin_loop.cond_stmt2),
  				 lhs1, lhs2, NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&stmts, cond_stmt);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
}

/* Create loop_latch BB for align_loop.
   eg: <bb 11>
       len_26 = len_17 + 8;

   The IR of bb is as above.  */

static void
create_align_loop_latch (basic_block &align_loop_latch, basic_block after_bb,
			 basic_block dominator_bb, class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;
  tree res;

  tree entry_node = get_current_def (origin_loop.base);
  align_loop_latch = create_empty_bb (after_bb);
  add_bb_to_loop (align_loop_latch, outer);
  make_edge (after_bb, align_loop_latch, EDGE_FALSE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, align_loop_latch, dominator_bb);
  gsi = gsi_last_bb (align_loop_latch);
  res = copy_ssa_name (entry_node);
  g = gimple_build_assign (res, PLUS_EXPR, entry_node,
			   build_int_cst (TREE_TYPE (entry_node), 8));
  gimple_seq_add_stmt (&stmts, g);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  defs_map.put (align_loop_latch, res);
}

/* Create a new loop and add it to outer_loop and return.  */

static class loop *
init_new_loop (class loop *outer_loop, basic_block header, basic_block latch)
{
  class loop *new_loop;
  new_loop = alloc_loop ();
  new_loop->header = header;
  new_loop->latch = latch;
  add_loop (new_loop, outer_loop);

  return new_loop;
}

/* Create necessary exit BB for align_loop.
   eg: <bb 12>
       _27 = _23 ^ _25;
       _28 = __builtin_ctzll (_27);
       _29 = _28 >> 3;
       len_30 = _29 + len_17;

   The IR of bb is as above.  */

static void
create_align_loop_exit_bb (basic_block &align_loop_exit_bb,
			   basic_block after_bb, basic_block dominator_bb,
			   class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;
  gimple *cond_stmt;
  tree lhs1, lhs2;
  tree cond_lhs, cond_rhs;
  gcall *build_ctzll;

  tree entry_node = get_current_def (origin_loop.base);
  align_loop_exit_bb = create_empty_bb (after_bb);
  add_bb_to_loop (align_loop_exit_bb, outer);
  make_edge (after_bb, align_loop_exit_bb, EDGE_TRUE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, align_loop_exit_bb, dominator_bb);
  gsi = gsi_last_bb (align_loop_exit_bb);

  cond_stmt = gsi_stmt (gsi_last_bb (after_bb));
  cond_lhs = gimple_cond_lhs (cond_stmt);
  cond_rhs = gimple_cond_rhs (cond_stmt);

  lhs1 = gimple_build (&stmts, BIT_XOR_EXPR, TREE_TYPE (cond_lhs), cond_lhs,
  		       cond_rhs);
  build_ctzll = gimple_build_call (builtin_decl_explicit (BUILT_IN_CTZLL), 1,
  				   lhs1);
  lhs1 = make_ssa_name (integer_type_node);
  gimple_call_set_lhs (build_ctzll, lhs1);
  gimple_seq_add_stmt (&stmts, build_ctzll);
  lhs2 = copy_ssa_name (lhs1);
  g = gimple_build_assign (lhs2, RSHIFT_EXPR, lhs1,
  			   build_int_cst (TREE_TYPE (lhs1), 3));
  gimple_seq_add_stmt (&stmts, g);
  lhs1 = gimple_build (&stmts, NOP_EXPR, TREE_TYPE (entry_node), lhs2);
  lhs2 = copy_ssa_name (entry_node);
  g = gimple_build_assign (lhs2, PLUS_EXPR, lhs1, entry_node);
  gimple_seq_add_stmt (&stmts, g);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  defs_map.put (align_loop_exit_bb, lhs2);
}

/* Create loop_header BB for epilogue_loop.
   eg: <bb 14>
       # len_31 = PHI <len_17 (13), len_37 (16)>
       if (len_31 != len_limit_12 (D))

   The IR of bb is as above.  */

static void
create_epilogue_loop_header (basic_block &epilogue_loop_header,
			     basic_block after_bb, basic_block dominator_bb,
			     class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gcond *cond_stmt;
  tree res;
  gphi *phi;

  tree entry_node = get_current_def (origin_loop.base);
  epilogue_loop_header = create_empty_bb (after_bb);
  add_bb_to_loop (epilogue_loop_header, outer);
  make_single_succ_edge (after_bb, epilogue_loop_header, EDGE_FALLTHRU);
  set_immediate_dominator (CDI_DOMINATORS, epilogue_loop_header, dominator_bb);
  gsi = gsi_last_bb (epilogue_loop_header);
  phi = create_phi_node (NULL_TREE, epilogue_loop_header);
  create_new_def_for (entry_node, phi, gimple_phi_result_ptr (phi));
  res = gimple_phi_result (phi);
  cond_stmt = gimple_build_cond (gimple_cond_code (origin_loop.cond_stmt1), res,
  				 origin_loop.limit, NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&stmts, cond_stmt);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);

  set_current_def (origin_loop.base, res);
  defs_map.put (epilogue_loop_header, res);
}

/* Create loop body BB for epilogue_loop.
   eg: <bb 15>
       _32 = (sizetype) len_31;
       _33 = pb_13 (D) + _32;
       _34 = *_33;
       _35 = cur_15 (D) + _32;
       _36 = *_35;
       if (_34 != _36)

   The IR of bb is as above.  */

static void
create_epilogue_loop_body_bb (basic_block &epilogue_loop_body_bb,
			      basic_block after_bb, basic_block dominator_bb,
			      class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;
  gcond *cond_stmt;
  tree lhs1, lhs2, lhs3;

  tree entry_node = get_current_def (origin_loop.base);
  epilogue_loop_body_bb = create_empty_bb (after_bb);
  add_bb_to_loop (epilogue_loop_body_bb, outer);
  make_edge (after_bb, epilogue_loop_body_bb, EDGE_TRUE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, epilogue_loop_body_bb, dominator_bb);
  gsi = gsi_last_bb (epilogue_loop_body_bb);
  lhs1 = gimple_build (&stmts, NOP_EXPR, sizetype, entry_node);
  lhs2 = gimple_build (&stmts, POINTER_PLUS_EXPR, TREE_TYPE (origin_loop.arr1),
  		       origin_loop.arr1, lhs1);
  g = gimple_build_assign (make_ssa_name (unsigned_char_type_node),
	fold_build2 (MEM_REF, unsigned_char_type_node, lhs2,
		     build_int_cst (TREE_TYPE (lhs2), 0)));
  gimple_seq_add_stmt (&stmts, g);
  lhs2 = gimple_assign_lhs (g);
  lhs3 = gimple_build (&stmts, POINTER_PLUS_EXPR, TREE_TYPE (origin_loop.arr2),
  		       origin_loop.arr2, lhs1);
  g = gimple_build_assign (make_ssa_name (unsigned_char_type_node),
  	fold_build2 (MEM_REF, unsigned_char_type_node, lhs3,
		     build_int_cst (TREE_TYPE (lhs3), 0)));
  gimple_seq_add_stmt (&stmts, g);
  lhs3 = gimple_assign_lhs (g);
  cond_stmt = gimple_build_cond (gimple_cond_code (origin_loop.cond_stmt2), lhs2,
  				 lhs3, NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&stmts, cond_stmt);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  defs_map.put (epilogue_loop_body_bb, get_current_def (origin_loop.base));
}

/* Create loop_latch BB for epilogue_loop.
   eg: <bb 16>
       len_37 = len_31 + 1;

   The IR of bb is as above.  */

static void
create_epilogue_loop_latch (basic_block &epilogue_loop_latch,
			    basic_block after_bb, basic_block dominator_bb,
			    class loop *outer)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi;
  gimple *g;
  tree res;

  tree entry_node = get_current_def (origin_loop.base);
  epilogue_loop_latch = create_empty_bb (after_bb);
  add_bb_to_loop (epilogue_loop_latch, outer);
  make_edge (after_bb, epilogue_loop_latch, EDGE_FALSE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, epilogue_loop_latch, dominator_bb);
  gsi = gsi_last_bb (epilogue_loop_latch);
  res = copy_ssa_name (entry_node);
  g = gimple_build_assign (res, PLUS_EXPR, entry_node,
	build_int_cst (TREE_TYPE (entry_node), origin_loop.step));
  gimple_seq_add_stmt (&stmts, g);
  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  defs_map.put (epilogue_loop_latch, res);
}

/* convert_to_new_loop
   |               |
   |               |
   |               | entry_edge
   |    ______     |
   |  /       V    V
   | |       -----origin_loop_header---
   | |      |                          |
   | |       -------------------------\
   | |        |                        \
   | |        V                         \___ ___ ___ ___ ___ ___ ___
   | |       -----origin_loop_body-----                             |
   | |      |                          |                            |
   | |       -------------------------\                             |
   | |        |                        \___ ___ ___ ___             |
   | |        V                                        V            V
   | |       -----origin_loop_latch----              -----exit_bb------
   | |      |                          |            |                  |
   | |      /--------------------------              ------------------
   |  \ __ /
   |
   |                         |
   |  ====>                  |entry_edge
   |                         V
   |                      -------prolog_bb-----
   |                     |                     |
   |                      ---------------------
   |                         |
   |                         V
   |                      -----align_loop_header----
   | /-----------------> |                          |
   |/                     --------------------------
   ||                        /                   \
   ||                       V                     V
   ||     ---align_loop_body---                ---epilogue_loop_header--
   ||    |                     |       -------|                         |<---|
   ||     --------------------\       /        -------------------------     |
   ||              |           \____  |                   |                  |
   ||              V                | |                   V                  |
   ||     ---align_loop_latch---    | |        ---epilogue_loop_body----     |
   ||    |                      |   | |   ----|                         |    |
   ||     ----------------------    | |  /     -------------------------     |
   ||              /     __________/  |  |                |                  |
   ||             /     |             |  |                V                  |
   | \ __________/      |             |  |     ---epilogue_loop_latch---     |
   |                    |             |  |    |                         |    |
   |                    |             |  |     -------------------------    /
   |                    V             |  |                |                /
   |        -align_loop_exit_bb-      |  |                 \______________/
   |       |                    |     |  |
   |        --------------------      |  |
   |                |                 |  |
   |                |                 V  V
   |                |      -----exit_bb------
   |                |---->|                  |
   |                       ------------------

   The origin_loop conversion process starts from entry_edge and ends at
   exit_bb; The execution logic of origin_loop is completely replaced by
   align_loop + epilogue_loop:
     1) align_loop mainly implements the idea of ​​using wide-type dereference
	and comparison on array elements, so as to achieve the effect of
	acceleration; For the corresponding source code understanding, please
	refer to the description of the pass at the beginning;
     2) epilogue_loop processes the previous loop remaining array element
	comparison.  */

static void
create_new_loops (edge entry_edge)
{
  basic_block prolog_bb;
  basic_block align_loop_header, align_loop_latch, align_loop_body_bb;
  basic_block align_pred_bb, align_loop_exit_bb;
  basic_block epilogue_loop_header, epilogue_loop_latch, epilogue_loop_body_bb;
  basic_block epilogue_loop_pred_bb;
  class loop *align_loop;
  class loop *epilogue_loop;

  class loop *outer = entry_edge->src->loop_father;

  create_prolog_bb (prolog_bb, entry_edge->src, entry_edge->src, outer,
  		    entry_edge);

  create_loop_pred_bb (align_pred_bb, prolog_bb, prolog_bb, outer);
  make_single_succ_edge (prolog_bb, align_pred_bb, EDGE_FALLTHRU);

  create_align_loop_header (align_loop_header, align_pred_bb,
  					align_pred_bb, outer);

  create_align_loop_body_bb (align_loop_body_bb, align_loop_header,
  			     align_loop_header, outer);

  create_align_loop_latch (align_loop_latch, align_loop_body_bb,
  			   align_loop_body_bb, outer);
  make_edge (align_loop_latch, align_loop_header, EDGE_FALLTHRU);
  rewrite_add_phi_arg (align_loop_header);

  align_loop = init_new_loop (outer, align_loop_header, align_loop_latch);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nPrint byte align loop %d:\n", align_loop->num);
      flow_loop_dump (align_loop, dump_file, NULL, 1);
      fprintf (dump_file, "\n\n");
    }

  create_align_loop_exit_bb (align_loop_exit_bb, align_loop_body_bb,
  			     align_loop_body_bb, outer);

  create_loop_pred_bb (epilogue_loop_pred_bb, align_loop_header,
  		       align_loop_header, outer);
  make_edge (align_loop_header, epilogue_loop_pred_bb, EDGE_FALSE_VALUE);

  create_epilogue_loop_header (epilogue_loop_header, epilogue_loop_pred_bb,
  			       epilogue_loop_pred_bb, outer);

  create_epilogue_loop_body_bb (epilogue_loop_body_bb, epilogue_loop_header,
  				epilogue_loop_header, outer);

  create_epilogue_loop_latch (epilogue_loop_latch, epilogue_loop_body_bb,
  			      epilogue_loop_body_bb, outer);
  make_single_succ_edge (epilogue_loop_latch, epilogue_loop_header,
  			 EDGE_FALLTHRU);
  rewrite_add_phi_arg (epilogue_loop_header);

  epilogue_loop = init_new_loop (outer, epilogue_loop_header,
  				 epilogue_loop_latch);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nPrint epilogue loop %d:\n", epilogue_loop->num);
      flow_loop_dump (epilogue_loop, dump_file, NULL, 1);
      fprintf (dump_file, "\n\n");
    }
  make_single_succ_edge (align_loop_exit_bb, origin_loop.exit_bb1,
  			 EDGE_FALLTHRU);
  set_immediate_dominator (CDI_DOMINATORS, origin_loop.exit_bb1,
  			   entry_edge->src);
  make_edge (epilogue_loop_body_bb, origin_loop.exit_bb1, EDGE_TRUE_VALUE);

  make_edge (epilogue_loop_header, origin_loop.exit_bb2, EDGE_FALSE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, origin_loop.exit_bb2,
  			   entry_edge->src);

  rewrite_add_phi_arg (origin_loop.exit_bb1);
  rewrite_add_phi_arg (origin_loop.exit_bb2);

  remove_edge (origin_loop.exit_e1);
  remove_edge (origin_loop.exit_e2);
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
      if (!imm_bb || bb == origin_loop.exit_bb1)
	{
	  set_immediate_dominator (CDI_DOMINATORS, bb,
	  			   recompute_dominator (CDI_DOMINATORS, bb));
	  continue;
	}
    }
}

/* Clear information about the original loop.  */

static void
remove_origin_loop (class loop *loop)
{
  basic_block *body;

  body = get_loop_body_in_dom_order (loop);
  unsigned n = loop->num_nodes;
  for (unsigned i = 0; i < n; i++)
    {
	  delete_basic_block (body[i]);
    }
  free (body);
  delete_loop (loop);
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

/* The main entry of array-widen-compare optimizes.  */

static unsigned int
tree_ssa_array_widen_compare ()
{
  unsigned int todo = 0;
  class loop *loop;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      flow_loops_dump (dump_file, NULL, 1);
      fprintf (dump_file, "\nConfirm which loop can be optimized using"
			  " array-widen-compare\n");
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

      if (determine_loop_form (loop))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "The %dth loop form is success matched,"
				  "and the loop can be optimized.\n",
		       loop->num);
	      dump_loop_bb (loop);
	    }

	  convert_to_new_loop (loop);
	}
    }

  todo |= (TODO_update_ssa);
  return todo;
}

/* Array widen compare.  */

namespace {

const pass_data pass_data_tree_array_widen_compare =
{
  GIMPLE_PASS,
  "awiden_compare",
  OPTGROUP_LOOP,
  TV_TREE_ARRAY_WIDEN_COMPARE,
  (PROP_cfg | PROP_ssa),
  0,
  0,
  0,
  (TODO_update_ssa | TODO_verify_all)
};

class pass_array_widen_compare : public gimple_opt_pass
{
public:
  pass_array_widen_compare (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_array_widen_compare, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *);

}; // class pass_array_widen_compare

bool
pass_array_widen_compare::gate (function *)
{
  return (flag_array_widen_compare > 0 && optimize >= 3);
}

unsigned int
pass_array_widen_compare::execute (function *fun)
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

  return tree_ssa_array_widen_compare ();
}

} // anon namespace

gimple_opt_pass *
make_pass_array_widen_compare (gcc::context *ctxt)
{
  return new pass_array_widen_compare (ctxt);
}
