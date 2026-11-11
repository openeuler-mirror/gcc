/* Coalesce canonical packed-bit loop nests into masked word stores.
   Copyright (C) 2026 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This opt-in pass recognizes a two-level loop nest that writes one bit at a
   time to packed 32-bit words.  The outer induction visits a permutation of
   the bit range and the inner induction visits rows.  The stored value is a
   row-invariant bit with one diagonal inversion.  A second packed array masks
   out non-writable bits.

   The fast path interchanges the loops, updates complete words with one
   masked read/modify/write with the diagonal inversion in its desired value.
   It retains presence and change tests to avoid introducing accesses.  Runtime
   access-range checks retain the original nest for overlapping writable,
   mask or row-index storage.  A read-only preflight also protects invariant
   metadata before inspecting a subsequent input row.  Recognition is based
   solely on CFG, SSA, induction, address and bit-operation properties.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "fold-const.h"
#include "tree-pass.h"
#include "options.h"
#include "context.h"
#include "function.h"
#include "basic-block.h"
#include "bitmap.h"
#include "dominance.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "cfgloopmanip.h"
#include "tree-ssa-loop.h"
#include "tree-ssa-loop-manip.h"
#include "tree-cfg.h"
#include "tree-cfgcleanup.h"
#include "tree-ssa.h"
#include "ssa.h"
#include "tree-phinodes.h"
#include "tree-into-ssa.h"
#include "builtins.h"
#include "alias.h"
#include "predict.h"
#include "gimple-predict.h"

/* A cheap shared structural screen.  Detailed matching must still prove
   every operation; block count is only a work bound, never an idiom key.  */
bool
bitset_store_coalescing_candidate_p (class loop *outer)
{
  class loop *inner = outer->inner;
  return loop_depth (outer) == 1 && inner && !inner->next && !inner->inner
	 && outer->num_nodes <= 16 && inner->num_nodes <= 16
	 && single_exit (outer) && single_exit (inner);
}

namespace {

/* Retain the conservative 16-bit profitability ceiling.  This limits each
   guarded row footprint to 2048 32-bit words (8192 bytes); larger ranges use
   the original loop.  It is a policy cap, not a distinct-storage proof.  */
const unsigned HOST_WIDE_INT MAX_FAST_BITS = 65535;

const pass_data bitset_store_coalescing_pass_data = {
  GIMPLE_PASS, "bitset_coalesce", OPTGROUP_LOOP, TV_TREE_BITSET_COALESCE,
  PROP_cfg | PROP_ssa, 0, 0, 0, 0
};

const pass_data bitset_store_coalescing_prepare_pass_data = {
  GIMPLE_PASS, "bitset_coalesce_prep", OPTGROUP_LOOP, TV_TREE_BITSET_PREPARE,
  PROP_cfg | PROP_ssa, 0, 0, 0, 0
};

struct canonical_iv
{
  gphi *phi;
  tree value;
  tree next;
  tree bound;
  edge exit;
  bool tests_next;
};

struct candidate
{
  class loop *outer;
  class loop *inner;
  canonical_iv outer_iv;
  canonical_iv inner_iv;
  tree literal;
  tree literal_base;
  tree info_base;
  tree presence_base;
  tree row_offset;
  tree rotation_mod;
  tree rotation_dividend;
  tree bit_mask;
  gassign *info_store;
  tree info_mem;
  tree presence_mem;
  gcond *presence_cond;
  gcond *desired_cond;
};

static bool
same_tree_p (tree a, tree b)
{
  return a && b && operand_equal_p (a, b, 0);
}

static bool
same_assignment_rhs_p (tree a, tree b)
{
  if (same_tree_p (a, b))
    return true;
  if (!a || !b || TREE_CODE (a) != SSA_NAME || TREE_CODE (b) != SSA_NAME
      || !types_compatible_p (TREE_TYPE (a), TREE_TYPE (b)))
    return false;
  gassign *a_def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (a));
  gassign *b_def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (b));
  if (!a_def || !b_def
      || gimple_assign_rhs_code (a_def) != gimple_assign_rhs_code (b_def)
      || gimple_num_ops (a_def) != gimple_num_ops (b_def))
    return false;
  for (unsigned int i = 1; i < gimple_num_ops (a_def); ++i)
    if (!same_tree_p (gimple_op (a_def, i), gimple_op (b_def, i)))
      return false;
  return true;
}

static bool
one_cst_p (tree value)
{
  return value && TREE_CODE (value) == INTEGER_CST && integer_onep (value);
}

static bool
zero_cst_p (tree value)
{
  return value && TREE_CODE (value) == INTEGER_CST && integer_zerop (value);
}

static bool value_depends_on_1 (tree, tree, unsigned, bitmap);

struct dependency_walk
{
  tree target;
  unsigned depth;
  bitmap visited;
  bool found;
};

static tree
dependency_walk_cb (tree *tp, int *walk_subtrees, void *data)
{
  dependency_walk *walk = static_cast<dependency_walk *> (data);
  if (TREE_CODE (*tp) == SSA_NAME)
    {
      if (value_depends_on_1 (*tp, walk->target, walk->depth + 1,
			      walk->visited))
	walk->found = true;
      *walk_subtrees = 0;
    }
  else if (TYPE_P (*tp))
    *walk_subtrees = 0;
  return walk->found ? *tp : NULL_TREE;
}

static bool
value_depends_on_1 (tree value, tree target, unsigned depth, bitmap visited)
{
  if (same_tree_p (value, target))
    return true;
  if (depth > 32 || !value || TREE_CODE (value) != SSA_NAME)
    return false;
  unsigned version = SSA_NAME_VERSION (value);
  if (bitmap_bit_p (visited, version))
    return false;
  bitmap_set_bit (visited, version);

  gimple *def = SSA_NAME_DEF_STMT (value);
  if (!def || gimple_nop_p (def))
    return false;
  if (gphi *phi = dyn_cast<gphi *> (def))
    {
      for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	if (value_depends_on_1 (gimple_phi_arg_def (phi, i), target,
			       depth + 1, visited))
	  return true;
      return false;
    }

  gassign *assign = dyn_cast<gassign *> (def);
  if (!assign)
    return false;
  for (unsigned i = 1; i < gimple_num_ops (assign); ++i)
    {
      tree op = gimple_op (assign, i);
      if (!op)
	continue;
      dependency_walk walk = { target, depth, visited, false };
      walk_tree_without_duplicates (&op, dependency_walk_cb, &walk);
      if (walk.found)
	return true;
    }
  return false;
}

static bool
value_depends_on_p (tree value, tree target)
{
  auto_bitmap visited;
  return value_depends_on_1 (value, target, 0, visited);
}

static bool
find_canonical_iv (class loop *loop, canonical_iv *out)
{
  edge pre = loop_preheader_edge (loop);
  edge latch = loop_latch_edge (loop);
  edge exit = single_exit (loop);
  if (!pre || !latch || !exit)
    return false;

  gimple_stmt_iterator last = gsi_last_bb (exit->src);
  if (gsi_end_p (last))
    return false;
  gcond *cond = dyn_cast<gcond *> (gsi_stmt (last));
  if (!cond)
    return false;

  for (gimple_stmt_iterator gsi = gsi_start_phis (loop->header);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = as_a<gphi *> (gsi_stmt (gsi));
      tree value = gimple_phi_result (phi);
      if (!INTEGRAL_TYPE_P (TREE_TYPE (value)))
	continue;
      tree initial = PHI_ARG_DEF_FROM_EDGE (phi, pre);
      tree next = PHI_ARG_DEF_FROM_EDGE (phi, latch);
      if (!zero_cst_p (initial) || TREE_CODE (next) != SSA_NAME)
	continue;
      gassign *inc = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (next));
      if (!inc || gimple_assign_rhs_code (inc) != PLUS_EXPR)
	continue;
      tree add0 = gimple_assign_rhs1 (inc);
      tree add1 = gimple_assign_rhs2 (inc);
      if (!((same_tree_p (add0, value) && one_cst_p (add1))
	    || (same_tree_p (add1, value) && one_cst_p (add0))))
	continue;

      enum tree_code code = gimple_cond_code (cond);
      tree lhs = gimple_cond_lhs (cond);
      tree rhs = gimple_cond_rhs (cond);
      bool exit_on_false = (exit->flags & EDGE_FALSE_VALUE) != 0;
      tree bound = NULL_TREE;
      if (exit_on_false && (code == LT_EXPR || code == NE_EXPR)
	  && (same_tree_p (lhs, value) || same_tree_p (lhs, next)))
	bound = rhs;
      else if (exit_on_false && code == GT_EXPR
	       && (same_tree_p (rhs, value) || same_tree_p (rhs, next)))
	bound = lhs;
      if (!bound || value_depends_on_p (bound, value))
	continue;

      /* The old IV is a trip count only at a top test.  A bottom test of
	 it executes one additional body.  A test of NEXT is accepted only
	 after the body, with the increment dominating that exit.  */
      bool tests_old = same_tree_p (lhs, value) || same_tree_p (rhs, value);
      if (tests_old)
	{
	  if (exit->src != loop->header)
	    continue;
	  bool pure_header = true;
	  for (gimple_stmt_iterator si = gsi_start_bb (loop->header);
	       !gsi_end_p (si); gsi_next (&si))
	    {
	      gimple *stmt = gsi_stmt (si);
	      if (stmt == cond || is_gimple_debug (stmt)
		  || gimple_code (stmt) == GIMPLE_LABEL
		  || gimple_code (stmt) == GIMPLE_PREDICT)
		continue;
	      if (!is_gimple_assign (stmt) || gimple_vdef (stmt)
		  || gimple_has_volatile_ops (stmt))
		pure_header = false;
	    }
	  if (!pure_header)
	    continue;
	}
      else if (exit->src == loop->header
	       || !dominated_by_p (CDI_DOMINATORS, loop->latch, exit->src)
	       || !dominated_by_p (CDI_DOMINATORS, exit->src,
				   gimple_bb (inc)))
	continue;

      out->phi = phi;
      out->value = value;
      out->next = next;
      out->bound = bound;
      out->exit = exit;
      out->tests_next = !tests_old;
      return true;
    }
  return false;
}

static bool
direct_memory_load_p (tree value, tree *mem)
{
  if (!value || TREE_CODE (value) != SSA_NAME)
    return false;
  gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (value));
  if (!def || !gimple_assign_single_p (def))
    return false;
  tree rhs = gimple_assign_rhs1 (def);
  if (!REFERENCE_CLASS_P (rhs))
    return false;
  if (mem)
    *mem = rhs;
  return true;
}

/* PRE and sinking commonly leave two SSA names for the same dynamic
   one-bit mask: one guards the presence load and the other feeds the XOR
   store.  Treat those names as equivalent only when both are canonical
   unsigned/signed one-left-shift expressions with the identical shift
   count.  */
static bool
same_one_bit_mask_p (tree a, tree b)
{
  if (same_tree_p (a, b))
    return true;
  if (!a || !b || TREE_CODE (a) != SSA_NAME || TREE_CODE (b) != SSA_NAME)
    return false;

  gassign *a_def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (a));
  gassign *b_def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (b));
  return a_def && b_def
	 && gimple_assign_rhs_code (a_def) == LSHIFT_EXPR
	 && gimple_assign_rhs_code (b_def) == LSHIFT_EXPR
	 && one_cst_p (gimple_assign_rhs1 (a_def))
	 && one_cst_p (gimple_assign_rhs1 (b_def))
	 && same_tree_p (gimple_assign_rhs2 (a_def),
			 gimple_assign_rhs2 (b_def));
}

static edge
true_or_false_edge (basic_block bb, bool want_true)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    if ((want_true && (e->flags & EDGE_TRUE_VALUE))
	|| (!want_true && (e->flags & EDGE_FALSE_VALUE)))
      return e;
  return NULL;
}

static edge
zero_edge (gcond *cond, tree value)
{
  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);
  if (!((same_tree_p (lhs, value) && zero_cst_p (rhs))
	|| (same_tree_p (rhs, value) && zero_cst_p (lhs))))
    return NULL;
  if (gimple_cond_code (cond) == EQ_EXPR)
    return true_or_false_edge (gimple_bb (cond), true);
  if (gimple_cond_code (cond) == NE_EXPR)
    return true_or_false_edge (gimple_bb (cond), false);
  return NULL;
}

static tree
memory_pointer (tree mem)
{
  /* The modeled SSA pointer already contains the complete bit address.
     A nonzero MEM_REF displacement would otherwise be lost both in the
     access-range guard and in the generated fast path.  */
  if (TREE_CODE (mem) != MEM_REF || !integer_zerop (TREE_OPERAND (mem, 1)))
    return NULL_TREE;
  return TREE_OPERAND (mem, 0);
}

static bool
split_pointer_plus (tree pointer, tree *base, tree *offset)
{
  if (!pointer || TREE_CODE (pointer) != SSA_NAME)
    return false;
  gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (pointer));
  if (!def || gimple_assign_rhs_code (def) != POINTER_PLUS_EXPR)
    return false;
  *base = gimple_assign_rhs1 (def);
  *offset = gimple_assign_rhs2 (def);
  return true;
}

/* Ignore integral copies/conversions only where the subsequent bit operation
   observes the low bits, or where the matched value is known to be 0/1.  */
static tree
strip_integral_copies (tree value)
{
  for (unsigned int depth = 0; value && TREE_CODE (value) == SSA_NAME
       && depth < 16; ++depth)
    {
      gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (value));
      if (!def)
	break;
      tree rhs = gimple_assign_rhs1 (def);
      if (gimple_assign_rhs_code (def) == SSA_NAME)
	value = rhs;
      else if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def))
	       && INTEGRAL_TYPE_P (TREE_TYPE (value))
	       && INTEGRAL_TYPE_P (TREE_TYPE (rhs))
	       && TYPE_PRECISION (TREE_TYPE (value))
		  >= TYPE_PRECISION (TREE_TYPE (rhs)))
	value = rhs;
      else
	break;
    }
  return value;
}

/* Unlike the low-bit helper above, address proofs require numeric values to
   survive every conversion.  NONNEGATIVE is valid for the canonical IV
   starting at zero and tested against a positive bound.  */
static tree
strip_value_preserving_copies (tree value, bool nonnegative = false)
{
  for (unsigned depth = 0; value && TREE_CODE (value) == SSA_NAME
       && depth < 16; ++depth)
    {
      gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (value));
      if (!def)
	break;
      enum tree_code code = gimple_assign_rhs_code (def);
      tree rhs = gimple_assign_rhs1 (def);

      if ((code != SSA_NAME && !CONVERT_EXPR_CODE_P (code))
	  || !INTEGRAL_TYPE_P (TREE_TYPE (value))
	  || !INTEGRAL_TYPE_P (TREE_TYPE (rhs)))
	break;
      tree to = TREE_TYPE (value), from = TREE_TYPE (rhs);
      unsigned to_bits = TYPE_PRECISION (to);
      unsigned from_bits = TYPE_PRECISION (from);
      if (to_bits < from_bits
	  || (TYPE_UNSIGNED (from) && !TYPE_UNSIGNED (to)
	      && to_bits == from_bits)
	  || (!TYPE_UNSIGNED (from) && TYPE_UNSIGNED (to)
	      && !nonnegative))
	break;
      value = rhs;
    }
  return value;
}

static gassign *
binary_definition (tree value, enum tree_code code)
{
  value = strip_integral_copies (value);
  if (!value || TREE_CODE (value) != SSA_NAME)
    return NULL;
  gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (value));
  return def && gimple_assign_rhs_code (def) == code ? def : NULL;
}

static bool
small_constant_p (tree value, unsigned HOST_WIDE_INT expected)
{
  return value && TREE_CODE (value) == INTEGER_CST
	 && tree_fits_uhwi_p (value) && tree_to_uhwi (value) == expected;
}

static bool
commutative_constant_operand (tree value, enum tree_code code,
			      unsigned HOST_WIDE_INT constant, tree *other)
{
  gassign *def = binary_definition (value, code);
  if (!def)
    return false;
  tree a = gimple_assign_rhs1 (def), b = gimple_assign_rhs2 (def);
  if (small_constant_p (b, constant))
    *other = a;
  else if (small_constant_p (a, constant))
    *other = b;
  else
    return false;
  return true;
}

/* Count occurrences of IV in a same-type addition tree.  An arbitrary
   dependency is not proof of the unit-stride permutation used by the fast
   path.  Invariant terms are retained, and the runtime guard below excludes
   negative dividends and wrapping additions over the whole iteration range.  */
static int
unit_stride_occurrences (tree value, tree iv, tree type, unsigned depth = 0)
{
  if (same_tree_p (value, iv))
    return 1;
  if (!value_depends_on_p (value, iv))
    return 0;
  if (depth > 24 || !value || TREE_CODE (value) != SSA_NAME)
    return -1;
  gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (value));
  if (!def || gimple_assign_rhs_code (def) != PLUS_EXPR
      || !types_compatible_p (TREE_TYPE (value), type)
      || !types_compatible_p (TREE_TYPE (gimple_assign_rhs1 (def)), type)
      || !types_compatible_p (TREE_TYPE (gimple_assign_rhs2 (def)), type))
    return -1;
  int a = unit_stride_occurrences (gimple_assign_rhs1 (def), iv, type,
				   depth + 1);
  int b = unit_stride_occurrences (gimple_assign_rhs2 (def), iv, type,
				   depth + 1);
  return a < 0 || b < 0 || a + b > 1 ? -1 : a + b;
}

static bool
desired_bit_p (tree value, tree desired_xor)
{
  tree xor_value;
  return commutative_constant_operand (value, BIT_AND_EXPR, 1, &xor_value)
	 && same_tree_p (strip_integral_copies (xor_value), desired_xor);
}

static bool
old_bit_p (tree value, tree old_info, tree shift)
{
  tree shifted;
  if (!commutative_constant_operand (value, BIT_AND_EXPR, 1, &shifted))
    return false;
  gassign *def = binary_definition (shifted, RSHIFT_EXPR);
  return def
	 && same_tree_p (gimple_assign_rhs1 (def), old_info)
	 && same_tree_p (strip_integral_copies (gimple_assign_rhs2 (def)),
			 strip_integral_copies (shift));
}

static bool
canonical_bit_index_p (candidate &cand, tree word_offset)
{
  gassign *mask_def = as_a<gassign *> (SSA_NAME_DEF_STMT (cand.bit_mask));
  tree bit_index, mod, scaled;
  if (!commutative_constant_operand (gimple_assign_rhs2 (mask_def),
				     BIT_AND_EXPR, 31, &bit_index)
      || !commutative_constant_operand (bit_index, PLUS_EXPR, 1, &mod))
    return false;
  mod = strip_integral_copies (mod);
  if (!mod || TREE_CODE (mod) != SSA_NAME)
    return false;
  gassign *mod_def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (mod));
  if (!mod_def
      || (gimple_assign_rhs_code (mod_def) != TRUNC_MOD_EXPR
	  && gimple_assign_rhs_code (mod_def) != FLOOR_MOD_EXPR)
      || !same_tree_p (gimple_assign_rhs2 (mod_def), cand.outer_iv.bound)
      || !types_compatible_p (TREE_TYPE (mod), TREE_TYPE (cand.outer_iv.bound)))
    return false;
  tree dividend = gimple_assign_rhs1 (mod_def);
  if (unit_stride_occurrences (dividend, cand.outer_iv.value,
			       TREE_TYPE (mod)) != 1)
    return false;
  if (!commutative_constant_operand (word_offset, MULT_EXPR, 4, &scaled))
    return false;
  gassign *word_def = binary_definition (scaled, RSHIFT_EXPR);
  if (!word_def || !small_constant_p (gimple_assign_rhs2 (word_def), 5)
      || !same_tree_p (strip_integral_copies (gimple_assign_rhs1 (word_def)),
		       strip_integral_copies (bit_index)))
    return false;
  cand.rotation_mod = mod;
  cand.rotation_dividend = dividend;
  return true;
}

/* The fallback loop is bypassed, not copied into the fast path.  Every
   observable operation must therefore be accounted for explicitly.  */
static bool
no_extra_side_effects_p (candidate &cand)
{
  bool safe = true;
  basic_block *bbs = get_loop_body (cand.outer);
  for (unsigned int i = 0; i < cand.outer->num_nodes && safe; ++i)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (is_gimple_debug (stmt) || gimple_code (stmt) == GIMPLE_PREDICT)
	  continue;
	if (gimple_code (stmt) == GIMPLE_COND
	    && stmt != *gsi_last_bb (cand.outer_iv.exit->src)
	    && stmt != *gsi_last_bb (cand.inner_iv.exit->src)
	    && stmt != cand.presence_cond && stmt != cand.desired_cond)
	  {
	    safe = false;
	    break;
	  }
	if (gimple_has_volatile_ops (stmt)
	    || (stmt != cand.info_store
		&& (gimple_vdef (stmt) || gimple_has_side_effects (stmt)))
	    || (!is_gimple_assign (stmt)
		&& gimple_code (stmt) != GIMPLE_COND
		&& gimple_code (stmt) != GIMPLE_LABEL
		&& gimple_code (stmt) != GIMPLE_GOTO
		&& gimple_code (stmt) != GIMPLE_NOP))
	  {
	    safe = false;
	    break;
	  }
      }
  free (bbs);
  return safe;
}


static bool
reject_candidate (class loop *inner, const char *reason)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "packed-bit candidate loop %d rejected: %s\n",
	     inner->num, reason);
  return false;
}

static bool
only_virtual_exit_phis_p (basic_block bb)
{
  for (gimple_stmt_iterator gsi = gsi_start_phis (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    if (!virtual_operand_p
	  (gimple_phi_result (as_a<gphi *> (gsi_stmt (gsi)))))
      return false;
  return true;
}

/* This early pass does not require LCSSA.  A scalar may escape the loop
   directly even when its exit has no scalar PHI.  Do not add a bypass unless
   all nondebug scalar uses remain within the original nest.  */
static bool
scalar_live_out_p (class loop *loop, auto_vec<gimple *> *debug_uses = nullptr)
{
  basic_block *body = get_loop_body (loop);
  bool escapes = false;
  for (unsigned i = 0; i < loop->num_nodes && !escapes; ++i)
    {
      auto_vec<tree> names;
      for (gphi_iterator pi = gsi_start_phis (body[i]); !gsi_end_p (pi);
	   gsi_next (&pi))
	names.safe_push (gimple_phi_result (pi.phi ()));
      for (gimple_stmt_iterator si = gsi_start_bb (body[i]); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  ssa_op_iter it;
	  tree name;
	  FOR_EACH_SSA_TREE_OPERAND (name, gsi_stmt (si), it, SSA_OP_DEF)
	    names.safe_push (name);
	}
      for (tree name : names)
	{
	  if (virtual_operand_p (name))
	    continue;
	  imm_use_iterator it;
	  use_operand_p use;
	  FOR_EACH_IMM_USE_FAST (use, it, name)
	    {
	      gimple *stmt = USE_STMT (use);
	      if (debug_uses && gimple_debug_bind_p (stmt)
		  && !flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
		debug_uses->safe_push (stmt);
	      if (!is_gimple_debug (stmt)
		  && !flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
		escapes = true;
	    }
	}
    }
  free (body);
  return escapes;
}

static bool
find_candidate (class loop *inner, candidate *out)
{
  class loop *outer = loop_outer (inner);
  if (!outer || loop_depth (outer) != 1
      || outer->inner != inner || inner->next)
    return false;
  if (!bitset_store_coalescing_candidate_p (outer))
    return reject_candidate (inner, "unsupported loop structure or work bound");
  if (!can_duplicate_loop_p (outer))
    return reject_candidate (inner, "loop duplication safety");

  candidate cand = {};
  cand.outer = outer;
  cand.inner = inner;
  if (!find_canonical_iv (outer, &cand.outer_iv))
    return reject_candidate (inner, "outer induction");
  if (!find_canonical_iv (inner, &cand.inner_iv))
    return reject_candidate (inner, "inner induction");
  if (flag_trapv || flag_non_call_exceptions
      || TYPE_PRECISION (unsigned_type_node) != 32
      || !types_compatible_p (TREE_TYPE (cand.outer_iv.value),
			      TREE_TYPE (cand.outer_iv.bound))
      || !types_compatible_p (TREE_TYPE (cand.inner_iv.value),
			      TREE_TYPE (cand.outer_iv.bound))
      || TYPE_PRECISION (TREE_TYPE (cand.outer_iv.value)) < 32
      || value_depends_on_p (cand.outer_iv.bound, cand.outer_iv.value)
      || (TREE_CODE (cand.outer_iv.bound) == SSA_NAME
	  && gimple_bb (SSA_NAME_DEF_STMT (cand.outer_iv.bound))
	  && flow_bb_inside_loop_p
	       (outer, gimple_bb (SSA_NAME_DEF_STMT (cand.outer_iv.bound)))))
    return reject_candidate (inner, "induction types");
  if (scalar_live_out_p (outer))
    return reject_candidate (inner, "scalar live-out");
  if (!only_virtual_exit_phis_p (cand.outer_iv.exit->dest))
    return reject_candidate (inner, "exit phi");

  calculate_dominance_info (CDI_DOMINATORS);

  tree equality = NULL_TREE;
  tree desired_xor = NULL_TREE;
  basic_block *bbs = get_loop_body (inner);
  for (unsigned i = 0; i < inner->num_nodes; ++i)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      if (gassign *assign = dyn_cast<gassign *> (gsi_stmt (gsi)))
	if (gimple_assign_rhs_code (assign) == EQ_EXPR)
	  {
	    tree a = gimple_assign_rhs1 (assign);
	    tree b = gimple_assign_rhs2 (assign);
	    if ((same_tree_p (a, cand.outer_iv.value)
		 && same_tree_p (b, cand.inner_iv.value))
		|| (same_tree_p (b, cand.outer_iv.value)
		    && same_tree_p (a, cand.inner_iv.value)))
	      equality = gimple_assign_lhs (assign);
	  }
  if (!equality)
    {
      free (bbs);
      return reject_candidate (inner, "diagonal equality");
    }

  for (unsigned i = 0; i < inner->num_nodes && !cand.literal; ++i)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      if (gassign *assign = dyn_cast<gassign *> (gsi_stmt (gsi)))
	if (gimple_assign_rhs_code (assign) == BIT_XOR_EXPR)
	  {
	    tree a = gimple_assign_rhs1 (assign);
	    tree b = gimple_assign_rhs2 (assign);
	    tree literal = NULL_TREE;
	    if (same_tree_p (strip_integral_copies (a), equality)
		&& direct_memory_load_p (b, NULL))
	      literal = b;
	    else if (same_tree_p (strip_integral_copies (b), equality)
		     && direct_memory_load_p (a, NULL))
	      literal = a;
	    if (literal && INTEGRAL_TYPE_P (TREE_TYPE (literal))
		&& TYPE_PRECISION (TREE_TYPE (literal)) == 32
		&& value_depends_on_p (literal, cand.inner_iv.value)
		&& !value_depends_on_p (literal, cand.outer_iv.value))
	      {
		cand.literal = literal;
		desired_xor = gimple_assign_lhs (assign);
	      }
	  }

  tree info_mem = NULL_TREE;
  tree old_info = NULL_TREE;
  for (unsigned i = 0; i < inner->num_nodes && !cand.info_store; ++i)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      if (gassign *store = dyn_cast<gassign *> (gsi_stmt (gsi)))
	{
	  tree lhs = gimple_assign_lhs (store);
	  tree rhs = gimple_assign_rhs1 (store);
	  if (!REFERENCE_CLASS_P (lhs) || TREE_CODE (rhs) != SSA_NAME)
	    continue;
	  gassign *xor_def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (rhs));
	  if (!xor_def || gimple_assign_rhs_code (xor_def) != BIT_XOR_EXPR)
	    continue;
	  tree xa = gimple_assign_rhs1 (xor_def);
	  tree xb = gimple_assign_rhs2 (xor_def);
	  tree old_mem = NULL_TREE;
	  tree mask = NULL_TREE;
	  if (direct_memory_load_p (xa, &old_mem) && operand_equal_p (old_mem, lhs, 0))
	    {
	      old_info = xa;
	      mask = xb;
	    }
	  else if (direct_memory_load_p (xb, &old_mem)
		   && operand_equal_p (old_mem, lhs, 0))
	    {
	      old_info = xb;
	      mask = xa;
	    }
	  if (!mask || TREE_CODE (mask) != SSA_NAME)
	    continue;
	  gassign *mask_def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (mask));
	  if (!mask_def || gimple_assign_rhs_code (mask_def) != LSHIFT_EXPR
	      || !one_cst_p (gimple_assign_rhs1 (mask_def))
	      || !value_depends_on_p (mask, cand.outer_iv.value)
	      || value_depends_on_p (mask, cand.inner_iv.value))
	    continue;
	  cand.info_store = store;
	  cand.bit_mask = mask;
	  info_mem = lhs;
	}

  /* Do not inspect control dependencies until the complete store
     pattern exists.  OLD_INFO alone can survive a rejected mask.  */
  if (!cand.literal || !desired_xor || !cand.info_store
      || !old_info || !info_mem)
    {
      free (bbs);
      return reject_candidate (inner, "incomplete bit store");
    }

  tree presence_mem = NULL_TREE;
  tree masked_presence = NULL_TREE;
  for (unsigned i = 0; i < inner->num_nodes && !presence_mem; ++i)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      if (gassign *assign = dyn_cast<gassign *> (gsi_stmt (gsi)))
	if (gimple_assign_rhs_code (assign) == BIT_AND_EXPR)
	  {
	    tree a = gimple_assign_rhs1 (assign);
	    tree b = gimple_assign_rhs2 (assign);
	    tree mem = NULL_TREE;
	    if (same_one_bit_mask_p (a, cand.bit_mask)
		&& direct_memory_load_p (b, &mem))
	      presence_mem = mem;
	    else if (same_one_bit_mask_p (b, cand.bit_mask)
		     && direct_memory_load_p (a, &mem))
	      presence_mem = mem;
	    if (presence_mem)
	      masked_presence = gimple_assign_lhs (assign);
	  }

  tree shift = gimple_assign_rhs2
    (as_a<gassign *> (SSA_NAME_DEF_STMT (cand.bit_mask)));
  bool presence_controls_store = false;
  bool desired_controls_store = false;
  for (unsigned i = 0; i < inner->num_nodes; ++i)
    {
      gimple_stmt_iterator gsi = gsi_last_bb (bbs[i]);
      if (gsi_end_p (gsi))
	continue;
      gcond *cond = dyn_cast<gcond *> (gsi_stmt (gsi));
      if (!cond)
	continue;
      if (edge zero = zero_edge (cond, masked_presence))
	if (single_pred_p (zero->dest)
	    && dominated_by_p (CDI_DOMINATORS, gimple_bb (cand.info_store),
			       zero->dest))
	  {
	    presence_controls_store = true;
	    cand.presence_cond = cond;
	  }

      tree a = gimple_cond_lhs (cond);
      tree b = gimple_cond_rhs (cond);
      bool related = ((desired_bit_p (a, desired_xor)
		       && old_bit_p (b, old_info, shift))
		      || (desired_bit_p (b, desired_xor)
			  && old_bit_p (a, old_info, shift)));
      if (!related || (gimple_cond_code (cond) != EQ_EXPR
		       && gimple_cond_code (cond) != NE_EXPR))
	continue;
      bool unequal_is_true = gimple_cond_code (cond) == NE_EXPR;
      edge store_edge = true_or_false_edge (gimple_bb (cond), unequal_is_true);
      if (store_edge && single_pred_p (store_edge->dest)
	  && dominated_by_p (CDI_DOMINATORS, gimple_bb (cand.info_store),
			     store_edge->dest))
	{
	  desired_controls_store = true;
	  cand.desired_cond = cond;
	}
    }
  free (bbs);

  if (!cand.literal || !cand.info_store || !presence_mem
      || !presence_controls_store || !desired_controls_store)
    return reject_candidate (inner, "bit update diamond");

  /* A NEXT comparison must follow the modeled iteration, not an early
     break before its payload.  Scalar SSA dominance of the increment alone
     does not prove that a row or an inner traversal has executed.  */
  if ((cand.inner_iv.tests_next
       && !dominated_by_p (CDI_DOMINATORS, cand.inner_iv.exit->src,
			   gimple_bb (SSA_NAME_DEF_STMT (cand.literal))))
      || (cand.outer_iv.tests_next
	  && !dominated_by_p (CDI_DOMINATORS, cand.outer_iv.exit->src,
			      inner->header)))
    return reject_candidate (inner, "iteration payload does not precede exit");

  if (!INTEGRAL_TYPE_P (TREE_TYPE (info_mem))
      || TYPE_PRECISION (TREE_TYPE (info_mem)) != 32
      || !INTEGRAL_TYPE_P (TREE_TYPE (presence_mem))
      || TYPE_PRECISION (TREE_TYPE (presence_mem)) != 32)
    return reject_candidate (inner, "packed word type");
  /* The generated references use unsigned int.  Do not silently strengthen
     an underaligned or may_alias access from the original program.  */
  if (reverse_storage_order_for_component_p (info_mem)
      || reverse_storage_order_for_component_p (presence_mem)
      || get_object_alignment (info_mem) < TYPE_ALIGN (unsigned_type_node)
      || get_object_alignment (presence_mem) < TYPE_ALIGN (unsigned_type_node)
      || TYPE_ALIGN (TREE_TYPE (info_mem)) < TYPE_ALIGN (unsigned_type_node)
      || TYPE_ALIGN (TREE_TYPE (presence_mem)) < TYPE_ALIGN (unsigned_type_node)
      || get_alias_set (info_mem) != get_alias_set (unsigned_type_node)
      || get_alias_set (presence_mem) != get_alias_set (unsigned_type_node))
    return reject_candidate (inner, "packed word alignment or alias type");

  tree info_ptr = memory_pointer (info_mem);
  tree presence_ptr = memory_pointer (presence_mem);
  tree info_offset = NULL_TREE;
  tree presence_offset = NULL_TREE;
  if (!split_pointer_plus (info_ptr, &cand.info_base, &info_offset)
      || !split_pointer_plus (presence_ptr, &cand.presence_base,
			      &presence_offset)
      || !same_assignment_rhs_p (info_offset, presence_offset)
      || same_tree_p (cand.info_base, cand.presence_base))
    return reject_candidate (inner, "paired packed-array addresses");

  if (TREE_CODE (info_offset) != SSA_NAME)
    return reject_candidate (inner, "packed-array SSA offset");
  gassign *offset_def
    = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (info_offset));
  if (!offset_def || gimple_assign_rhs_code (offset_def) != PLUS_EXPR)
    return reject_candidate (inner, "packed-array row/word offset");
  tree oa = gimple_assign_rhs1 (offset_def);
  tree ob = gimple_assign_rhs2 (offset_def);
  if (value_depends_on_p (oa, cand.literal)
      && !value_depends_on_p (oa, cand.outer_iv.value)
      && value_depends_on_p (ob, cand.outer_iv.value)
      && !value_depends_on_p (ob, cand.literal))
    cand.row_offset = oa;
  else if (value_depends_on_p (ob, cand.literal)
	   && !value_depends_on_p (ob, cand.outer_iv.value)
	   && value_depends_on_p (oa, cand.outer_iv.value)
	   && !value_depends_on_p (oa, cand.literal))
    cand.row_offset = ob;
  else
    return reject_candidate (inner, "row offset dependence");

  tree word_offset = same_tree_p (oa, cand.row_offset) ? ob : oa;

  tree literal_mem = NULL_TREE;
  if (!direct_memory_load_p (cand.literal, &literal_mem))
    return reject_candidate (inner, "literal load");
  tree literal_ptr = memory_pointer (literal_mem);
  tree literal_offset = NULL_TREE;
  if (!split_pointer_plus (literal_ptr, &cand.literal_base,
			   &literal_offset)
      || !value_depends_on_p (literal_offset, cand.inner_iv.value)
      || value_depends_on_p (cand.literal_base, cand.inner_iv.value))
    return reject_candidate (inner, "literal address");

  /* The preflight guard reads this contiguous input before any writes.
     Arbitrary indexed input requires a different access-range proof.  */
  tree byte_offset = strip_value_preserving_copies (literal_offset);
  gassign *byte_product
    = byte_offset && TREE_CODE (byte_offset) == SSA_NAME
      ? dyn_cast<gassign *> (SSA_NAME_DEF_STMT (byte_offset)) : NULL;
  tree literal_index;
  if (!byte_product
      || gimple_assign_rhs_code (byte_product) != MULT_EXPR
      || !TYPE_UNSIGNED (TREE_TYPE (byte_offset))
      || TYPE_PRECISION (TREE_TYPE (byte_offset))
	 != TYPE_PRECISION (size_type_node)
      || !commutative_constant_operand (byte_offset, MULT_EXPR, 4,
				       &literal_index)
      || !same_tree_p (strip_value_preserving_copies (literal_index, true),
		      cand.inner_iv.value))
    return reject_candidate (inner, "noncontiguous literal input");

  if (!canonical_bit_index_p (cand, word_offset))
    return reject_candidate (inner, "canonical rotating bit index");
  if (!no_extra_side_effects_p (cand))
    return reject_candidate (inner, "unmodeled side effects");

  cand.info_mem = info_mem;
  cand.presence_mem = presence_mem;
  *out = cand;
  return true;
}

struct replacement
{
  tree from;
  tree to;
};

struct clone_map
{
  tree from;
  tree to;
};

struct clone_context
{
  class loop *scope;
  gimple_seq *seq;
  auto_vec<replacement, 8> replacements;
  auto_vec<clone_map, 32> maps;
  unsigned depth;
  bool failed;
};

static tree clone_value (tree, clone_context *);

static tree
clone_tree_cb (tree *tp, int *walk_subtrees, void *data)
{
  clone_context *ctx = static_cast<clone_context *> (data);
  if (TREE_CODE (*tp) == SSA_NAME)
    {
      tree cloned = clone_value (*tp, ctx);
      if (!cloned)
	ctx->failed = true;
      else
	*tp = cloned;
      *walk_subtrees = 0;
    }
  else if (TYPE_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

static tree
clone_operand (tree operand, clone_context *ctx)
{
  operand = unshare_expr (operand);
  walk_tree (&operand, clone_tree_cb, ctx, NULL);
  return ctx->failed ? NULL_TREE : operand;
}

static tree
clone_value (tree value, clone_context *ctx)
{
  for (unsigned i = 0; i < ctx->replacements.length (); ++i)
    if (same_tree_p (value, ctx->replacements[i].from))
      return ctx->replacements[i].to;
  if (TREE_CODE (value) != SSA_NAME)
    return value;
  for (unsigned i = 0; i < ctx->maps.length (); ++i)
    if (same_tree_p (value, ctx->maps[i].from))
      return ctx->maps[i].to;
  if (ctx->depth > 40)
    return NULL_TREE;

  gimple *def = SSA_NAME_DEF_STMT (value);
  if (!def || gimple_nop_p (def) || !gimple_bb (def)
      || !flow_bb_inside_loop_p (ctx->scope, gimple_bb (def)))
    return value;
  gassign *assign = dyn_cast<gassign *> (def);
  if (!assign)
    return NULL_TREE;

  ctx->depth++;
  tree op1 = clone_operand (gimple_assign_rhs1 (assign), ctx);
  tree op2 = NULL_TREE;
  tree op3 = NULL_TREE;
  enum gimple_rhs_class rhs_class
    = get_gimple_rhs_class (gimple_assign_rhs_code (assign));
  if (rhs_class == GIMPLE_BINARY_RHS || rhs_class == GIMPLE_TERNARY_RHS)
    op2 = clone_operand (gimple_assign_rhs2 (assign), ctx);
  if (rhs_class == GIMPLE_TERNARY_RHS)
    op3 = clone_operand (gimple_assign_rhs3 (assign), ctx);
  ctx->depth--;
  if (ctx->failed || !op1
      || ((rhs_class == GIMPLE_BINARY_RHS || rhs_class == GIMPLE_TERNARY_RHS)
	  && !op2)
      || (rhs_class == GIMPLE_TERNARY_RHS && !op3))
    return NULL_TREE;

  tree result = make_ssa_name (TREE_TYPE (value));
  gassign *copy;
  if (rhs_class == GIMPLE_SINGLE_RHS)
    copy = gimple_build_assign (result, op1);
  else if (rhs_class == GIMPLE_UNARY_RHS)
    copy = gimple_build_assign (result, gimple_assign_rhs_code (assign), op1);
  else if (rhs_class == GIMPLE_BINARY_RHS)
    copy = gimple_build_assign (result, gimple_assign_rhs_code (assign),
				op1, op2);
  else
    copy = gimple_build_assign (result, gimple_assign_rhs_code (assign),
				op1, op2, op3);
  gimple_seq_add_stmt (ctx->seq, copy);
  ctx->maps.safe_push ({ value, result });
  return result;
}

static tree
ssa_assign1 (gimple_seq *seq, tree type, enum tree_code code, tree rhs)
{
  tree lhs = make_ssa_name (type);
  gimple_seq_add_stmt (seq, gimple_build_assign (lhs, code, rhs));
  return lhs;
}

static tree
ssa_assign2 (gimple_seq *seq, tree type, enum tree_code code,
	     tree rhs1, tree rhs2)
{
  tree lhs = make_ssa_name (type);
  gimple_seq_add_stmt (seq, gimple_build_assign (lhs, code, rhs1, rhs2));
  return lhs;
}

static tree
ssa_assign3 (gimple_seq *seq, tree type, enum tree_code code,
	     tree rhs1, tree rhs2, tree rhs3)
{
  tree lhs = make_ssa_name (type);
  gimple_seq_add_stmt (seq,
		       gimple_build_assign (lhs, code, rhs1, rhs2, rhs3));
  return lhs;
}

static void
insert_seq (basic_block bb, gimple_seq seq)
{
  gimple_stmt_iterator gsi = gsi_after_labels (bb);
  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
}

static basic_block
new_bb_after (basic_block after, class loop *father)
{
  basic_block bb = create_empty_bb (after);
  bb->count = after->count;
  add_bb_to_loop (bb, father);
  return bb;
}

static tree
build_mem_ref (tree pointer)
{
  return build_simple_mem_ref (pointer);
}

static tree
bool_and (gimple_seq *seq, tree a, tree b)
{
  return ssa_assign2 (seq, boolean_type_node, BIT_AND_EXPR, a, b);
}

static tree
clone_for_index (candidate &cand, tree value, tree old_index,
		 tree new_index, gimple_seq *seq,
		 tree old_base = NULL_TREE, tree new_base = NULL_TREE,
		 tree old_literal = NULL_TREE, tree new_literal = NULL_TREE)
{
  clone_context ctx = {};
  ctx.scope = cand.outer;
  ctx.seq = seq;
  ctx.depth = 0;
  ctx.failed = false;
  ctx.replacements.safe_push ({ old_index, new_index });
  if (old_base)
    ctx.replacements.safe_push ({ old_base, new_base });
  if (old_literal)
    ctx.replacements.safe_push ({ old_literal, new_literal });
  tree result = clone_value (value, &ctx);
  return ctx.failed ? NULL_TREE : result;
}

/* Own the detached statements until every potentially failing clone has
   succeeded.  Failed matching/cloning must not leave edges, blocks or SSA
   definitions in the live function.  */
struct pending_bitset_sequences
{
  gimple_seq init = NULL, guard = NULL, outer = NULL, scan = NULL;
  tree index = NULL_TREE, rotation = NULL_TREE;
  tree scan_index = NULL_TREE;
  bool committed = false;

  ~pending_bitset_sequences ()
  {
    if (!committed)
      {
	gimple_seq_discard (outer);
	gimple_seq_discard (scan);
	gimple_seq_discard (guard);
	gimple_seq_discard (init);
	if (index)
	  release_ssa_name (index);
	if (rotation)
	  release_ssa_name (rotation);
	if (scan_index)
	  release_ssa_name (scan_index);
      }
  }
};

struct guarded_read
{
  tree first;
  tree last;
};

/* Every load other than the three modeled data accesses must be a scalar
   loop-invariant metadata read available on the first inner iteration.
   Cloning its address without replacing either induction rejects indexed
   loads and loop-carried PHIs.  Protect these cells before advancing the
   preflight scan: an original store could otherwise change a future input
   pointer or bound, making an eager scan introduce invalid accesses.  */
static bool
collect_guarded_reads (candidate &cand, clone_context *ctx,
		       auto_vec<guarded_read, 8> &reads, tree *valid)
{
  basic_block body = gimple_bb (SSA_NAME_DEF_STMT (cand.literal));
  basic_block *bbs = get_loop_body (cand.outer);
  bool safe = true;
  for (unsigned i = 0; i < cand.outer->num_nodes && safe; ++i)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gassign *stmt = dyn_cast<gassign *> (gsi_stmt (gsi));
	if (!stmt || !gimple_assign_load_p (stmt))
	  continue;
	tree mem = gimple_assign_rhs1 (stmt);
	if (same_tree_p (gimple_assign_lhs (stmt), cand.literal)
	    || operand_equal_p (mem, cand.info_mem, 0)
	    || operand_equal_p (mem, cand.presence_mem, 0))
	  continue;
	HOST_WIDE_INT bytes = int_size_in_bytes (TREE_TYPE (mem));
	if ((!INTEGRAL_TYPE_P (TREE_TYPE (mem))
	     && !POINTER_TYPE_P (TREE_TYPE (mem)))
	    || bytes <= 0 || TREE_CODE (mem) == BIT_FIELD_REF
	    || (TREE_CODE (mem) == COMPONENT_REF
		&& DECL_BIT_FIELD_TYPE (TREE_OPERAND (mem, 1)))
	    || !dominated_by_p (CDI_DOMINATORS, body, bbs[i]))
	  {
	    safe = false;
	    break;
	  }
	tree address = clone_operand (build_fold_addr_expr (mem), ctx);
	if (!address || ctx->failed)
	  {
	    safe = false;
	    break;
	  }
	/* A conversion cannot take &p->field directly in unary GIMPLE.  */
	gimple_seq address_seq = NULL;
	address = force_gimple_operand_1 (address, &address_seq, is_gimple_reg,
					  NULL_TREE);
	gimple_seq_add_seq (ctx->seq, address_seq);
	tree first = ssa_assign1 (ctx->seq, size_type_node, NOP_EXPR, address);
	tree last = ssa_assign2 (ctx->seq, size_type_node, PLUS_EXPR, first,
				 build_int_cst (size_type_node, bytes));
	reads.safe_push ({first, last});
	tree no_wrap = ssa_assign2 (ctx->seq, boolean_type_node, GE_EXPR,
				   last, first);
	*valid = bool_and (ctx->seq, *valid, no_wrap);
      }
  free (bbs);
  return safe;
}

/* Bound a nonnegative-offset window starting at BASE so that it cannot
   intersect [FIRST, LAST).  A region entirely below BASE imposes no bound;
   otherwise stop at FIRST.  Reject a BASE inside the forbidden region.
   Together with nonwrapping endpoints, each row fitting in this window is
   disjoint without repeating every metadata comparison for every row.  */
static tree
access_window (gimple_seq *seq, tree base, tree first, tree last, tree *valid)
{
  tree above = ssa_assign2 (seq, boolean_type_node, LE_EXPR, last, base);
  tree before = ssa_assign2 (seq, boolean_type_node, LE_EXPR, base, first);
  tree outside = ssa_assign2 (seq, boolean_type_node, BIT_IOR_EXPR,
			     above, before);
  *valid = bool_and (seq, *valid, outside);
  tree distance = ssa_assign2 (seq, size_type_node, MINUS_EXPR, first, base);
  return ssa_assign3 (seq, size_type_node, COND_EXPR, above,
		      TYPE_MAX_VALUE (size_type_node), distance);
}

/* Estimate a useful row alignment from invariant multiplicative factors.
   This is only a profitability choice: the runtime checks below prove that
   EVERY actual offset is a multiple of it and that it spans a complete row.
   Thus neither narrowing conversions nor wrapping arithmetic in the original
   expression can invalidate the row-disjointness proof.  */
static tree
row_alignment (tree value, candidate &cand, clone_context *ctx,
	       unsigned depth = 0)
{
  if (depth > 40)
    return build_one_cst (size_type_node);
  value = strip_integral_copies (value);
  if (!value_depends_on_p (value, cand.literal))
    {
      tree factor = clone_value (value, ctx);
      if (!factor || ctx->failed)
	return NULL_TREE;
      return ssa_assign1 (ctx->seq, size_type_node, NOP_EXPR, factor);
    }
  if (TREE_CODE (value) == SSA_NAME)
    if (gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (value)))
      if (gimple_assign_rhs_code (def) == MULT_EXPR)
	{
	  tree a = row_alignment (gimple_assign_rhs1 (def), cand, ctx,
				  depth + 1);
	  tree b = row_alignment (gimple_assign_rhs2 (def), cand, ctx,
				  depth + 1);
	  if (!a || !b)
	    return NULL_TREE;
	  return ssa_assign2 (ctx->seq, size_type_node, MULT_EXPR, a, b);
	}
  return build_one_cst (size_type_node);
}

static bool
transform_candidate (candidate &cand)
{
  edge original_pre = loop_preheader_edge (cand.outer);
  edge original_exit = cand.outer_iv.exit;
  if (!original_pre || !original_exit)
    return false;

  tree index_type = TREE_TYPE (cand.inner_iv.value);
  tree limit_type = TREE_TYPE (cand.outer_iv.bound);
  tree u32 = unsigned_type_node;
  tree u32_ptr = build_pointer_type (u32);
  tree all_ones = build_all_ones_cst (u32);

  pending_bitset_sequences pending;
  gimple_seq &init_seq = pending.init;
  gimple_seq &guard_seq = pending.guard;
  gimple_seq &outer_seq = pending.outer;
  clone_context init_ctx = {};
  init_ctx.scope = cand.outer;
  init_ctx.seq = &init_seq;
  tree row_count = clone_value (cand.inner_iv.bound, &init_ctx);
  if (!row_count || init_ctx.failed)
    return false;
  clone_context base_ctx = {};
  base_ctx.scope = cand.outer;
  base_ctx.seq = &guard_seq;
  tree info_base = clone_value (cand.info_base, &base_ctx);
  tree presence_base = clone_value (cand.presence_base, &base_ctx);
  tree literal_base = clone_value (cand.literal_base, &base_ctx);
  if (!info_base || !presence_base || !literal_base || base_ctx.failed)
    return false;

  clone_context rotation_ctx = {};
  rotation_ctx.scope = cand.outer;
  rotation_ctx.seq = &guard_seq;
  rotation_ctx.replacements.safe_push
    ({ cand.outer_iv.value, build_zero_cst (limit_type) });
  tree initial_dividend = clone_value (cand.rotation_dividend, &rotation_ctx);
  tree initial_rotation = clone_value (cand.rotation_mod, &rotation_ctx);
  if (!initial_dividend || !initial_rotation || rotation_ctx.failed)
    return false;

  auto_vec<guarded_read, 8> reads;
  tree metadata_valid = boolean_true_node;
  if (!collect_guarded_reads (cand, &base_ctx, reads, &metadata_valid))
    return false;
  tree row_stride = row_alignment (cand.row_offset, cand, &base_ctx);
  if (!row_stride || base_ctx.failed)
    return false;

  tree scan_i = pending.scan_index = make_ssa_name (index_type);
  tree scan_literal
    = clone_for_index (cand, cand.literal, cand.inner_iv.value, scan_i,
		       &pending.scan, cand.literal_base, literal_base);
  if (!scan_literal)
    return false;
  tree scan_offset
    = clone_for_index (cand, cand.row_offset, cand.inner_iv.value, scan_i,
		       &pending.scan, cand.literal_base, literal_base,
		       cand.literal, scan_literal);
  if (!scan_offset)
    return false;

  tree fast_i = pending.index = make_ssa_name (index_type);
  tree bit_mod = pending.rotation = make_ssa_name (limit_type);
  tree literal
    = clone_for_index (cand, cand.literal, cand.inner_iv.value, fast_i,
		       &outer_seq, cand.literal_base, literal_base);
  if (!literal)
    return false;
  tree row_offset
    = clone_for_index (cand, cand.row_offset, cand.inner_iv.value, fast_i,
		       &outer_seq, cand.literal_base, literal_base,
		       cand.literal, literal);
  if (!row_offset)
    return false;

  /* All failure exits precede the first CFG mutation.  */
  pending.committed = true;
  basic_block dispatch = split_edge (original_pre);
  edge entry = single_pred_edge (dispatch);
  class loop *father = loop_outer (cand.outer);
  basic_block limit_guard = new_bb_after (entry->src, father);
  basic_block init = new_bb_after (limit_guard, father);
  basic_block guard = new_bb_after (init, father);
  basic_block scan_header = new_bb_after (guard, father);
  basic_block scan_latch = new_bb_after (scan_header, father);
  basic_block outer_header = new_bb_after (scan_latch, father);
  basic_block word_header = new_bb_after (outer_header, father);
  basic_block word_load = new_bb_after (word_header, father);
  basic_block word_store = new_bb_after (word_load, father);
  basic_block word_latch = new_bb_after (word_store, father);
  basic_block outer_latch = new_bb_after (word_latch, father);
  basic_block finish = new_bb_after (outer_latch, father);
  edge redirected = redirect_edge_and_branch (entry, limit_guard);
  gcc_assert (redirected);

  tree info_u32 = ssa_assign1 (&guard_seq, u32_ptr, NOP_EXPR, info_base);
  tree presence_u32
    = ssa_assign1 (&guard_seq, u32_ptr, NOP_EXPR, presence_base);
  tree literal_raw
    = ssa_assign1 (&guard_seq, ptr_type_node, NOP_EXPR, literal_base);
  /* Preserve both zero-trip tests before evaluating any body-only loads.  */
  gimple_seq limit_seq = NULL;
  gimple_seq_add_stmt
    (&limit_seq, gimple_build_cond (GT_EXPR, cand.outer_iv.bound,
				    build_zero_cst (limit_type),
				    NULL_TREE, NULL_TREE));
  insert_seq (limit_guard, limit_seq);
  tree count_positive
    = ssa_assign2 (&init_seq, boolean_type_node, GT_EXPR,
		   row_count, build_zero_cst (index_type));
  gimple_seq_add_stmt
    (&init_seq, gimple_build_cond (NE_EXPR, count_positive,
				   boolean_false_node, NULL_TREE, NULL_TREE));
  insert_seq (init, init_seq);

  tree i_last
    = ssa_assign2 (&guard_seq, limit_type, RSHIFT_EXPR,
		   cand.outer_iv.bound, build_int_cst (limit_type, 5));
  tree info_addr
    = ssa_assign1 (&guard_seq, size_type_node, NOP_EXPR, info_u32);
  tree presence_addr
    = ssa_assign1 (&guard_seq, size_type_node, NOP_EXPR, presence_u32);
  tree literal_addr
    = ssa_assign1 (&guard_seq, size_type_node, NOP_EXPR, literal_raw);
  tree info_presence_distinct
    = ssa_assign2 (&guard_seq, boolean_type_node, NE_EXPR,
		   info_addr, presence_addr);
  tree info_literal_distinct
    = ssa_assign2 (&guard_seq, boolean_type_node, NE_EXPR,
		   info_addr, literal_addr);
  tree bounded
    = ssa_assign2 (&guard_seq, boolean_type_node, LE_EXPR,
		   cand.outer_iv.bound,
		   build_int_cst (limit_type, MAX_FAST_BITS));
  tree fast_ok = bool_and (&guard_seq, info_presence_distinct,
			  info_literal_distinct);
  fast_ok = bool_and (&guard_seq, fast_ok, bounded);
  fast_ok = bool_and (&guard_seq, fast_ok, metadata_valid);

  tree count_size = ssa_assign1 (&guard_seq, size_type_node, NOP_EXPR,
				row_count);
  tree count_roundtrip = ssa_assign1 (&guard_seq, index_type, NOP_EXPR,
				     count_size);
  tree count_fits = ssa_assign2 (&guard_seq, boolean_type_node, EQ_EXPR,
				 count_roundtrip, row_count);
  tree literal_bytes = ssa_assign2 (&guard_seq, size_type_node, MULT_EXPR,
				    count_size,
				    build_int_cst (size_type_node, 4));
  tree count_check = ssa_assign2 (&guard_seq, size_type_node, RSHIFT_EXPR,
				  literal_bytes,
				  build_int_cst (size_type_node, 2));
  tree bytes_fit = ssa_assign2 (&guard_seq, boolean_type_node, EQ_EXPR,
				count_check, count_size);
  tree literal_end = ssa_assign2 (&guard_seq, size_type_node, PLUS_EXPR,
				   literal_addr, literal_bytes);
  tree literal_no_wrap = ssa_assign2 (&guard_seq, boolean_type_node, GE_EXPR,
				       literal_end, literal_addr);
  fast_ok = bool_and (&guard_seq, fast_ok, count_fits);
  fast_ok = bool_and (&guard_seq, fast_ok, bytes_fit);
  fast_ok = bool_and (&guard_seq, fast_ok, literal_no_wrap);

  tree words_size = ssa_assign1 (&guard_seq, size_type_node, NOP_EXPR, i_last);
  tree row_words = ssa_assign2 (&guard_seq, size_type_node, PLUS_EXPR,
				words_size, build_int_cst (size_type_node, 1));
  tree row_bytes = ssa_assign2 (&guard_seq, size_type_node, MULT_EXPR,
				row_words, build_int_cst (size_type_node, 4));
  tree stride_sufficient = ssa_assign2 (&guard_seq, boolean_type_node,
				      GE_EXPR, row_stride, row_bytes);
  fast_ok = bool_and (&guard_seq, fast_ok, stride_sufficient);

  /* Every selected row must fit below the other packed-array base.  Since
     offsets are nonnegative, this separates the complete writable and mask
     footprints even when literal values select arbitrary rows.  Also bound
     both end addresses against wrap and the writable window against the
     complete literal stream and each invariant metadata cell.  */
  tree low_base = ssa_assign2 (&guard_seq, size_type_node, MIN_EXPR,
			      info_addr, presence_addr);
  tree high_base = ssa_assign2 (&guard_seq, size_type_node, MAX_EXPR,
			       info_addr, presence_addr);
  tree capacity = ssa_assign2 (&guard_seq, size_type_node, MINUS_EXPR,
			      high_base, low_base);
  tree info_room = ssa_assign2 (&guard_seq, size_type_node, MINUS_EXPR,
			       TYPE_MAX_VALUE (size_type_node), info_addr);
  tree presence_room = ssa_assign2 (&guard_seq, size_type_node, MINUS_EXPR,
				   TYPE_MAX_VALUE (size_type_node),
				   presence_addr);
  capacity = ssa_assign2 (&guard_seq, size_type_node, MIN_EXPR,
			capacity, info_room);
  capacity = ssa_assign2 (&guard_seq, size_type_node, MIN_EXPR,
			capacity, presence_room);
  tree literal_room = access_window (&guard_seq, info_addr, literal_addr,
				    literal_end, &fast_ok);
  capacity = ssa_assign2 (&guard_seq, size_type_node, MIN_EXPR,
			capacity, literal_room);
  for (unsigned i = 0; i < reads.length (); ++i)
    {
      tree room = access_window (&guard_seq, info_addr, reads[i].first,
				 reads[i].last, &fast_ok);
      capacity = ssa_assign2 (&guard_seq, size_type_node, MIN_EXPR,
			    capacity, room);
    }
  tree row_fits = ssa_assign2 (&guard_seq, boolean_type_node, GE_EXPR,
			     capacity, row_bytes);
  fast_ok = bool_and (&guard_seq, fast_ok, row_fits);
  tree last_offset = ssa_assign2 (&guard_seq, size_type_node, MINUS_EXPR,
				capacity, row_bytes);

  /* A unit-stride addition tree is a permutation only if no intermediate
     wrapping can change its remainder.  The original first-iteration
     expression computes INITIAL_DIVIDEND; guard its complete future range.  */
  tree nonnegative
    = ssa_assign2 (&guard_seq, boolean_type_node, GE_EXPR,
		   initial_dividend, build_zero_cst (limit_type));
  tree distance
    = ssa_assign2 (&guard_seq, limit_type, MINUS_EXPR,
		   cand.outer_iv.bound, build_int_cst (limit_type, 1));
  tree room
    = ssa_assign2 (&guard_seq, limit_type, MINUS_EXPR,
		   TYPE_MAX_VALUE (limit_type), distance);
  tree no_wrap
    = ssa_assign2 (&guard_seq, boolean_type_node, LE_EXPR,
		   initial_dividend, room);
  fast_ok = bool_and (&guard_seq, fast_ok, nonnegative);
  fast_ok = bool_and (&guard_seq, fast_ok, no_wrap);
  tree limit_u32
    = ssa_assign1 (&guard_seq, u32, NOP_EXPR, cand.outer_iv.bound);
  tree not_limit = ssa_assign1 (&guard_seq, u32, BIT_NOT_EXPR, limit_u32);
  tree tail_shift
    = ssa_assign2 (&guard_seq, u32, BIT_AND_EXPR, not_limit,
		   build_int_cst (u32, 31));
  tree tail_mask
    = ssa_assign2 (&guard_seq, u32, RSHIFT_EXPR, all_ones, tail_shift);
  gimple_seq_add_stmt
    (&guard_seq, gimple_build_cond (NE_EXPR, fast_ok, boolean_false_node,
				    NULL_TREE, NULL_TREE));
  insert_seq (guard, guard_seq);

  /* Read the original contiguous literal stream, but no packed data.  Before
     advancing, prove that this row cannot change a future literal or any
     metadata address/bound.  On a failure, resume the untouched original
     nest immediately rather than speculating further input reads.  */
  gphi *scan_phi = create_phi_node (scan_i, scan_header);
  gimple_seq &scan_seq = pending.scan;
  tree offset = ssa_assign1 (&scan_seq, size_type_node, NOP_EXPR, scan_offset);
  tree scan_ok = ssa_assign2 (&scan_seq, boolean_type_node, LE_EXPR,
			     offset, last_offset);
  /* Different multiples of a stride at least ROW_BYTES apart cannot overlap.
     Equal offsets remain valid, preserving duplicate-row semantics.  The
     stride test above also excludes zero before this remainder executes.  */
  tree remainder = ssa_assign2 (&scan_seq, size_type_node, TRUNC_MOD_EXPR,
			       offset, row_stride);
  tree aligned = ssa_assign2 (&scan_seq, boolean_type_node, EQ_EXPR,
			     remainder, build_zero_cst (size_type_node));
  scan_ok = bool_and (&scan_seq, scan_ok, aligned);
  gimple_seq_add_stmt
    (&scan_seq, gimple_build_cond (NE_EXPR, scan_ok, boolean_false_node,
				  NULL_TREE, NULL_TREE));
  insert_seq (scan_header, scan_seq);

  gimple_seq scan_latch_seq = NULL;
  tree scan_next = ssa_assign2 (&scan_latch_seq, index_type, PLUS_EXPR,
			       scan_i, build_int_cst (index_type, 1));
  tree scan_more = ssa_assign2 (&scan_latch_seq, boolean_type_node, LT_EXPR,
			       scan_next, row_count);
  gimple_seq_add_stmt
    (&scan_latch_seq, gimple_build_cond (NE_EXPR, scan_more, boolean_false_node,
					NULL_TREE, NULL_TREE));
  insert_seq (scan_latch, scan_latch_seq);

  gphi *fast_i_phi = create_phi_node (fast_i, outer_header);
  gphi *bit_mod_phi = create_phi_node (bit_mod, outer_header);
  tree row_offset_size
    = ssa_assign1 (&outer_seq, size_type_node, NOP_EXPR, row_offset);
  tree info_row
    = ssa_assign2 (&outer_seq, u32_ptr, POINTER_PLUS_EXPR,
		   info_u32, row_offset_size);
  tree presence_row
    = ssa_assign2 (&outer_seq, u32_ptr, POINTER_PLUS_EXPR,
		   presence_u32, row_offset_size);
  tree literal_bit
    = ssa_assign2 (&outer_seq, TREE_TYPE (literal), BIT_AND_EXPR,
		   literal, build_int_cst (TREE_TYPE (literal), 1));
  tree literal_bit_u32
    = ssa_assign1 (&outer_seq, u32, NOP_EXPR, literal_bit);
  tree value = ssa_assign1 (&outer_seq, u32, NEGATE_EXPR, literal_bit_u32);
  tree diagonal_bit
    = ssa_assign2 (&outer_seq, limit_type, PLUS_EXPR, bit_mod,
		   build_int_cst (limit_type, 1));
  tree wrapped
    = ssa_assign2 (&outer_seq, boolean_type_node, EQ_EXPR,
		   diagonal_bit, cand.outer_iv.bound);
  tree bit_mod_next
    = ssa_assign3 (&outer_seq, limit_type, COND_EXPR,
		   wrapped, build_zero_cst (limit_type), diagonal_bit);
  tree diag_word
    = ssa_assign2 (&outer_seq, limit_type, RSHIFT_EXPR,
		   diagonal_bit, build_int_cst (limit_type, 5));
  tree diag_shift
    = ssa_assign2 (&outer_seq, limit_type, BIT_AND_EXPR,
		   diagonal_bit, build_int_cst (limit_type, 31));
  tree diag_mask
    = ssa_assign2 (&outer_seq, u32, LSHIFT_EXPR,
		   build_int_cst (u32, 1), diag_shift);
  tree has_diagonal
    = ssa_assign2 (&outer_seq, boolean_type_node, LT_EXPR,
		   fast_i, cand.outer_iv.bound);
  insert_seq (outer_header, outer_seq);

  tree word = make_ssa_name (limit_type);
  gphi *word_phi = create_phi_node (word, word_header);
  gimple_seq word_seq = NULL;
  tree word_size
    = ssa_assign1 (&word_seq, size_type_node, NOP_EXPR, word);
  tree word_offset
    = ssa_assign2 (&word_seq, size_type_node, MULT_EXPR, word_size,
		   build_int_cst (size_type_node, 4));
  tree info_word
    = ssa_assign2 (&word_seq, u32_ptr, POINTER_PLUS_EXPR,
		   info_row, word_offset);
  tree presence_word
    = ssa_assign2 (&word_seq, u32_ptr, POINTER_PLUS_EXPR,
		   presence_row, word_offset);
  tree presence_value = make_ssa_name (u32);
  gimple_seq_add_stmt
    (&word_seq,
     gimple_build_assign (presence_value, build_mem_ref (presence_word)));
  tree first
    = ssa_assign2 (&word_seq, boolean_type_node, EQ_EXPR, word,
		   build_zero_cst (limit_type));
  tree first_u32 = ssa_assign1 (&word_seq, u32, NOP_EXPR, first);
  tree first_mask
    = ssa_assign2 (&word_seq, u32, BIT_XOR_EXPR, all_ones, first_u32);
  tree last
    = ssa_assign2 (&word_seq, boolean_type_node, EQ_EXPR, word, i_last);
  tree last_u32 = ssa_assign1 (&word_seq, u32, NOP_EXPR, last);
  tree last_neg = ssa_assign1 (&word_seq, u32, NEGATE_EXPR, last_u32);
  tree not_tail = ssa_assign1 (&word_seq, u32, BIT_NOT_EXPR, tail_mask);
  tree tail_clear
    = ssa_assign2 (&word_seq, u32, BIT_AND_EXPR, last_neg, not_tail);
  tree last_mask = ssa_assign1 (&word_seq, u32, BIT_NOT_EXPR, tail_clear);
  tree mask
    = ssa_assign2 (&word_seq, u32, BIT_AND_EXPR, first_mask, last_mask);
  tree not_presence
    = ssa_assign1 (&word_seq, u32, BIT_NOT_EXPR, presence_value);
  tree writable
    = ssa_assign2 (&word_seq, u32, BIT_AND_EXPR, mask, not_presence);
  gimple_seq_add_stmt
    (&word_seq, gimple_build_cond (NE_EXPR, writable, build_zero_cst (u32),
				   NULL_TREE, NULL_TREE));
  insert_seq (word_header, word_seq);

  /* First preserve the original presence test.  Then form the complete final
     desired word, including the diagonal, and only write actual changes.
     A base fill followed by a diagonal XOR would introduce transient writes
     even when the source never stores anything.  */
  gimple_seq load_seq = NULL;
  tree old_value = make_ssa_name (u32);
  gimple_seq_add_stmt
    (&load_seq, gimple_build_assign (old_value, build_mem_ref (info_word)));
  tree diagonal_here
    = ssa_assign2 (&load_seq, boolean_type_node, EQ_EXPR, word, diag_word);
  diagonal_here = bool_and (&load_seq, has_diagonal, diagonal_here);
  tree word_diag
    = ssa_assign3 (&load_seq, u32, COND_EXPR, diagonal_here,
		   diag_mask, build_zero_cst (u32));
  tree desired = ssa_assign2 (&load_seq, u32, BIT_XOR_EXPR, value, word_diag);
  tree changed_bits
    = ssa_assign2 (&load_seq, u32, BIT_XOR_EXPR, old_value, desired);
  changed_bits
    = ssa_assign2 (&load_seq, u32, BIT_AND_EXPR, changed_bits, writable);
  tree new_value
    = ssa_assign2 (&load_seq, u32, BIT_XOR_EXPR, old_value, changed_bits);
  gimple_seq_add_stmt
    (&load_seq, gimple_build_cond (NE_EXPR, changed_bits, build_zero_cst (u32),
				   NULL_TREE, NULL_TREE));
  insert_seq (word_load, load_seq);
  gimple_seq store_seq = NULL;
  gimple_seq_add_stmt
    (&store_seq, gimple_build_assign (build_mem_ref (info_word), new_value));
  insert_seq (word_store, store_seq);

  gimple_seq word_latch_seq = NULL;
  tree word_next
    = ssa_assign2 (&word_latch_seq, limit_type, PLUS_EXPR, word,
		   build_int_cst (limit_type, 1));
  tree more_words
    = ssa_assign2 (&word_latch_seq, boolean_type_node, LE_EXPR,
		   word_next, i_last);
  gimple_seq_add_stmt
    (&word_latch_seq, gimple_build_cond (NE_EXPR, more_words,
					 boolean_false_node,
					 NULL_TREE, NULL_TREE));
  insert_seq (word_latch, word_latch_seq);
  gimple_seq latch_seq = NULL;
  tree fast_i_next
    = ssa_assign2 (&latch_seq, index_type, PLUS_EXPR, fast_i,
		   build_int_cst (index_type, 1));
  tree more_rows
    = ssa_assign2 (&latch_seq, boolean_type_node, LT_EXPR,
		   fast_i_next, row_count);
  gimple_seq_add_stmt
    (&latch_seq, gimple_build_cond (NE_EXPR, more_rows, boolean_false_node,
				    NULL_TREE, NULL_TREE));
  insert_seq (outer_latch, latch_seq);

  edge limit_yes = make_edge (limit_guard, init, EDGE_TRUE_VALUE);
  edge limit_no = make_edge (limit_guard, dispatch, EDGE_FALSE_VALUE);
  limit_yes->probability = profile_probability::likely ();
  limit_no->probability = limit_yes->probability.invert ();
  edge init_guard = make_edge (init, guard, EDGE_TRUE_VALUE);
  edge init_fallback = make_edge (init, dispatch, EDGE_FALSE_VALUE);
  init_guard->probability = profile_probability::likely ();
  init_fallback->probability = init_guard->probability.invert ();
  edge guard_scan = make_edge (guard, scan_header, EDGE_TRUE_VALUE);
  edge guard_fallback = make_edge (guard, dispatch, EDGE_FALSE_VALUE);
  guard_scan->probability = profile_probability::likely ();
  guard_fallback->probability = guard_scan->probability.invert ();
  edge scan_valid = make_edge (scan_header, scan_latch, EDGE_TRUE_VALUE);
  edge scan_fallback = make_edge (scan_header, dispatch, EDGE_FALSE_VALUE);
  scan_valid->probability = profile_probability::likely ();
  scan_fallback->probability = scan_valid->probability.invert ();
  edge scan_back = make_edge (scan_latch, scan_header, EDGE_TRUE_VALUE);
  edge guard_fast = make_edge (scan_latch, outer_header, EDGE_FALSE_VALUE);
  scan_back->probability = profile_probability::likely ();
  guard_fast->probability = scan_back->probability.invert ();
  edge outer_word = make_edge (outer_header, word_header, EDGE_FALLTHRU);
  outer_word->probability = profile_probability::always ();
  edge load_yes = make_edge (word_header, word_load, EDGE_TRUE_VALUE);
  edge load_no = make_edge (word_header, word_latch, EDGE_FALSE_VALUE);
  load_yes->probability = profile_probability::likely ();
  load_no->probability = load_yes->probability.invert ();
  edge store_yes = make_edge (word_load, word_store, EDGE_TRUE_VALUE);
  edge store_no = make_edge (word_load, word_latch, EDGE_FALSE_VALUE);
  store_yes->probability = profile_probability::likely ();
  store_no->probability = store_yes->probability.invert ();
  make_edge (word_store, word_latch, EDGE_FALLTHRU)->probability
    = profile_probability::always ();
  edge word_back = make_edge (word_latch, word_header, EDGE_TRUE_VALUE);
  edge word_done = make_edge (word_latch, outer_latch, EDGE_FALSE_VALUE);
  word_back->probability = profile_probability::likely ();
  word_done->probability = word_back->probability.invert ();
  edge outer_back = make_edge (outer_latch, outer_header, EDGE_TRUE_VALUE);
  edge outer_done = make_edge (outer_latch, finish, EDGE_FALSE_VALUE);
  outer_back->probability = profile_probability::likely ();
  outer_done->probability = outer_back->probability.invert ();
  make_edge (finish, original_exit->dest, EDGE_FALLTHRU)->probability
    = profile_probability::always ();

  add_phi_arg (fast_i_phi, build_zero_cst (index_type), guard_fast,
	       UNKNOWN_LOCATION);
  add_phi_arg (fast_i_phi, fast_i_next, outer_back, UNKNOWN_LOCATION);
  add_phi_arg (bit_mod_phi, initial_rotation, guard_fast, UNKNOWN_LOCATION);
  add_phi_arg (bit_mod_phi, bit_mod_next, outer_back, UNKNOWN_LOCATION);
  add_phi_arg (word_phi, build_zero_cst (limit_type), outer_word,
	       UNKNOWN_LOCATION);
  add_phi_arg (word_phi, word_next, word_back, UNKNOWN_LOCATION);
  add_phi_arg (scan_phi, build_zero_cst (index_type), guard_scan,
	       UNKNOWN_LOCATION);
  add_phi_arg (scan_phi, scan_next, scan_back, UNKNOWN_LOCATION);

  if (dump_file)
    fprintf (dump_file,
	     "created guarded packed-bit word-store fast path in %s "
	     "for loops %d/%d (max bits %llu)\n",
	     function_name (cfun), cand.outer->num, cand.inner->num,
	     (unsigned long long) MAX_FAST_BITS);
  /* Real live-outs were rejected, but debug-only uses must not constrain
     code generation.  Their fallback definitions no longer dominate them.  */
  auto_vec<gimple *> debug_uses;
  bool live_out = scalar_live_out_p (cand.outer, &debug_uses);
  gcc_assert (!live_out);
  for (gimple *stmt : debug_uses)
    {
      gimple_debug_bind_reset_value (stmt);
      update_stmt (stmt);
    }
  mark_virtual_operands_for_renaming (cfun);
  return true;
}


/* Normalize potential pattern loop nests before regular IPA inlining.  A
   continue predictor can occupy an otherwise empty block until the late
   predictor cleanup, after IPA has already introduced scalar exit PHIs.
   Remove predictor statements only from the narrow candidate shape and
   request CFG cleanup.  LIM without store motion then exposes the same
   address and loop-bound form used by the matcher.  The available LIM entry
   operates on the whole function, even if the shaped nest is later rejected.
   Keep this opt-in preparation tied to the CH option; do not silently run it
   when the user has disabled header copying.  */
class pass_bitset_store_coalescing_prepare : public gimple_opt_pass
{
public:
  explicit pass_bitset_store_coalescing_prepare (gcc::context *ctxt)
    : gimple_opt_pass (bitset_store_coalescing_prepare_pass_data, ctxt)
  {}

  opt_pass *clone () final override
  {
    return new pass_bitset_store_coalescing_prepare (m_ctxt);
  }

  bool gate (function *) final override
  {
    return flag_tree_bitset_store_coalescing && flag_tree_loop_im && flag_tree_ch
	   && optimize >= 3;
  }

  unsigned int execute (function *fun) final override
  {
    loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
    bool need_lim = false;
    bool removed_predict = false;
    for (auto inner : loops_list (fun, LI_FROM_INNERMOST))
      {
	class loop *outer = loop_outer (inner);
	if (!outer || !bitset_store_coalescing_candidate_p (outer))
	  continue;
	need_lim = true;

	basic_block *body = get_loop_body (inner);
	for (unsigned int i = 0; i < inner->num_nodes; ++i)
	  for (gimple_stmt_iterator gsi = gsi_start_bb (body[i]);
	       !gsi_end_p (gsi);)
	    if (gimple_code (gsi_stmt (gsi)) == GIMPLE_PREDICT
	&& gimple_predict_predictor (gsi_stmt (gsi)) == PRED_CONTINUE)
	      {
		gsi_remove (&gsi, true);
		removed_predict = true;
	      }
	    else
	      gsi_next (&gsi);
	free (body);
      }

    unsigned int todo = 0;
    if (need_lim)
      todo = loop_invariant_motion_in_fun (fun, false);
    loop_optimizer_finalize ();
    if (removed_predict)
      todo |= TODO_cleanup_cfg;
    return todo;
  }
};

class pass_bitset_store_coalescing : public gimple_opt_pass
{
public:
  explicit pass_bitset_store_coalescing (gcc::context *ctxt)
    : gimple_opt_pass (bitset_store_coalescing_pass_data, ctxt)
  {}

  bool gate (function *) final override
  {
    return flag_tree_bitset_store_coalescing && optimize >= 3;
  }

  unsigned int execute (function *fun) final override
  {
    loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
    candidate cand;
    bool changed = false;
    for (auto loop : loops_list (fun, LI_FROM_INNERMOST))
      if (find_candidate (loop, &cand))
	{
	  changed = transform_candidate (cand);
	  if (changed)
	    break;
	  reject_candidate (loop, "candidate transform rejected before CFG mutation");
	}
    if (changed)
      {
	/* New natural loops must be represented before any checking or a
	   preserved-loop consumer can inspect the updated CFG.  Early passes
	   usually discard the loop tree in finalize, but PROP_loops does not.  */
	free_dominance_info (CDI_DOMINATORS);
	calculate_dominance_info (CDI_DOMINATORS);
	loops_state_set (LOOPS_NEED_FIXUP);
	fix_loop_structure (NULL);
	checking_verify_loop_structure ();
      }
    loop_optimizer_finalize ();
    if (changed)
      {
	free_dominance_info (CDI_DOMINATORS);
	update_ssa (TODO_update_ssa);
	cleanup_tree_cfg ();
	if (loops_for_fn (fun))
	  {
	    calculate_dominance_info (CDI_DOMINATORS);
	    loops_state_set (LOOPS_NEED_FIXUP);
	    fix_loop_structure (NULL);
	    checking_verify_loop_structure ();
	  }
	checking_verify_ssa (true, true);
      }
    /* Detached loads can request virtual SSA renaming even when
       a later clone fails and their sequences are discarded.  */
    return need_ssa_update_p (fun) ? TODO_update_ssa : 0;
  }
};

} // anonymous namespace

gimple_opt_pass *
make_pass_bitset_store_coalescing_prepare (gcc::context *ctxt)
{
  return new pass_bitset_store_coalescing_prepare (ctxt);
}

gimple_opt_pass *
make_pass_bitset_store_coalescing (gcc::context *ctxt)
{
  return new pass_bitset_store_coalescing (ctxt);
}
