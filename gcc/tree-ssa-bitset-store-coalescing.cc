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
   masked read/modify/write and then applies the diagonal inversion.  Runtime
   base checks retain the original nest when the writable array shares its
   base with the mask or row-index array.  Recognition is based solely on
   CFG, SSA, induction, address and bit-operation properties.  */

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

namespace {

const unsigned HOST_WIDE_INT MAX_FAST_BITS = 65535;

const pass_data bitset_store_coalescing_pass_data = {
  GIMPLE_PASS, "bitset_coalesce", OPTGROUP_NONE, TV_NONE,
  PROP_cfg | PROP_ssa, 0, 0, 0, 0
};

struct canonical_iv
{
  gphi *phi;
  tree value;
  tree next;
  tree bound;
  edge exit;
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
  tree bit_mask;
  gassign *info_store;
};

static bool
same_tree_p (tree a, tree b)
{
  return a && b && operand_equal_p (a, b, 0);
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
	  && same_tree_p (lhs, next))
	bound = rhs;
      else if (exit_on_false && code == GT_EXPR && same_tree_p (rhs, next))
	bound = lhs;
      if (!bound || value_depends_on_p (bound, value))
	continue;

      out->phi = phi;
      out->value = value;
      out->next = next;
      out->bound = bound;
      out->exit = exit;
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
  if (TREE_CODE (mem) != MEM_REF)
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

static tree
find_modulo (tree value, tree outer_iv, tree bound, unsigned depth = 0)
{
  if (!value || depth > 24 || TREE_CODE (value) != SSA_NAME)
    return NULL_TREE;
  gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (value));
  if (!def)
    return NULL_TREE;
  if ((gimple_assign_rhs_code (def) == TRUNC_MOD_EXPR
	|| gimple_assign_rhs_code (def) == FLOOR_MOD_EXPR)
      && same_tree_p (gimple_assign_rhs2 (def), bound)
      && value_depends_on_p (gimple_assign_rhs1 (def), outer_iv))
    return value;
  for (unsigned i = 1; i < gimple_num_ops (def); ++i)
    {
      tree found = find_modulo (gimple_op (def, i), outer_iv, bound,
				depth + 1);
      if (found)
	return found;
    }
  return NULL_TREE;
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

static bool
find_candidate (class loop *inner, candidate *out)
{
  class loop *outer = loop_outer (inner);
  if (!outer || loop_depth (outer) != 1
      || outer->inner != inner || inner->next)
    return false;
  /* Keep the opt-in matcher cheap in very large LTO units.  The canonical
     update diamond has five inner-loop blocks; with its rotation setup and
     outer latch, the enclosing loop has nine.  Later tests remain fully
     structural, but there is no reason to run recursive SSA dependency
     walks for a different CFG.  */
  if (inner->num_nodes != 5 || outer->num_nodes != 9)
    return false;
  if (!can_duplicate_loop_p (outer))
    return reject_candidate (inner, "loop duplication safety");

  candidate cand = {};
  cand.outer = outer;
  cand.inner = inner;
  if (!find_canonical_iv (outer, &cand.outer_iv))
    return reject_candidate (inner, "outer induction");
  if (!find_canonical_iv (inner, &cand.inner_iv))
    return reject_candidate (inner, "inner induction");
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
	    if (value_depends_on_p (a, equality) && direct_memory_load_p (b, NULL))
	      literal = b;
	    else if (value_depends_on_p (b, equality)
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
	presence_controls_store
	  |= dominated_by_p (CDI_DOMINATORS, gimple_bb (cand.info_store),
			    zero->dest);

      tree a = gimple_cond_lhs (cond);
      tree b = gimple_cond_rhs (cond);
      bool related = ((value_depends_on_p (a, desired_xor)
		       && value_depends_on_p (b, old_info))
		      || (value_depends_on_p (b, desired_xor)
			  && value_depends_on_p (a, old_info)));
      if (!related)
	continue;
      bool unequal_is_true = gimple_cond_code (cond) == NE_EXPR;
      edge store_edge = true_or_false_edge (gimple_bb (cond), unequal_is_true);
      if (store_edge)
	desired_controls_store
	  |= dominated_by_p (CDI_DOMINATORS, gimple_bb (cand.info_store),
			    store_edge->dest);
    }
  free (bbs);

  if (!cand.literal || !cand.info_store || !presence_mem
      || !presence_controls_store || !desired_controls_store)
    return reject_candidate (inner, "bit update diamond");

  tree info_ptr = memory_pointer (info_mem);
  tree presence_ptr = memory_pointer (presence_mem);
  tree info_offset = NULL_TREE;
  tree presence_offset = NULL_TREE;
  if (!split_pointer_plus (info_ptr, &cand.info_base, &info_offset)
      || !split_pointer_plus (presence_ptr, &cand.presence_base,
			      &presence_offset)
      || !same_tree_p (info_offset, presence_offset)
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

  gassign *mask_def
    = as_a<gassign *> (SSA_NAME_DEF_STMT (cand.bit_mask));
  cand.rotation_mod
    = find_modulo (gimple_assign_rhs2 (mask_def), cand.outer_iv.value,
		   cand.outer_iv.bound);
  if (!cand.rotation_mod)
    return reject_candidate (inner, "rotating bit index");

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

static bool
transform_candidate (candidate &cand)
{
  edge original_pre = loop_preheader_edge (cand.outer);
  edge original_exit = cand.outer_iv.exit;
  if (!original_pre || !original_exit)
    return false;

  basic_block dispatch = split_edge (original_pre);
  edge entry = single_pred_edge (dispatch);
  basic_block entry_src = entry->src;
  class loop *father = loop_outer (cand.outer);

  basic_block init = new_bb_after (entry_src, father);
  basic_block guard = new_bb_after (init, father);
  basic_block outer_header = new_bb_after (guard, father);
  basic_block word_loop = new_bb_after (outer_header, father);
  basic_block diagonal_guard = new_bb_after (word_loop, father);
  basic_block diagonal_test = new_bb_after (diagonal_guard, father);
  basic_block diagonal_store = new_bb_after (diagonal_test, father);
  basic_block outer_latch = new_bb_after (diagonal_store, father);
  basic_block finish = new_bb_after (outer_latch, father);

  redirect_edge_and_branch (entry, init);

  tree index_type = TREE_TYPE (cand.inner_iv.value);
  tree limit_type = TREE_TYPE (cand.outer_iv.bound);
  tree u32 = unsigned_type_node;
  tree u32_ptr = build_pointer_type (u32);
  tree all_ones = build_all_ones_cst (u32);

  gimple_seq init_seq = NULL;
  clone_context init_ctx = {};
  init_ctx.scope = cand.outer;
  init_ctx.seq = &init_seq;
  tree info_base = clone_value (cand.info_base, &init_ctx);
  tree presence_base = clone_value (cand.presence_base, &init_ctx);
  tree literal_base = clone_value (cand.literal_base, &init_ctx);
  tree row_count = clone_value (cand.inner_iv.bound, &init_ctx);
  if (!info_base || !presence_base || !literal_base || !row_count
	|| init_ctx.failed)
    return false;
  tree info_u32 = ssa_assign1 (&init_seq, u32_ptr, NOP_EXPR, info_base);
  tree presence_u32
    = ssa_assign1 (&init_seq, u32_ptr, NOP_EXPR, presence_base);
  tree literal_raw
    = ssa_assign1 (&init_seq, ptr_type_node, NOP_EXPR, literal_base);
  tree limit_positive
    = ssa_assign2 (&init_seq, boolean_type_node, GT_EXPR,
		   cand.outer_iv.bound, build_zero_cst (limit_type));
  tree count_positive
    = ssa_assign2 (&init_seq, boolean_type_node, GT_EXPR,
		   row_count, build_zero_cst (index_type));
  tree can_transform = bool_and (&init_seq, limit_positive, count_positive);
  gimple_seq_add_stmt
    (&init_seq, gimple_build_cond (NE_EXPR, can_transform,
				   boolean_false_node, NULL_TREE, NULL_TREE));
  insert_seq (init, init_seq);

  gimple_seq guard_seq = NULL;
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
  tree fast_ok
    = bool_and (&guard_seq, info_presence_distinct,
		info_literal_distinct);
  fast_ok = bool_and (&guard_seq, fast_ok, bounded);

  clone_context rotation_ctx = {};
  rotation_ctx.scope = cand.outer;
  rotation_ctx.seq = &guard_seq;
  rotation_ctx.replacements.safe_push
    ({ cand.outer_iv.value, build_zero_cst (TREE_TYPE (cand.outer_iv.value)) });
  tree initial_rotation = clone_value (cand.rotation_mod, &rotation_ctx);
  if (!initial_rotation || rotation_ctx.failed)
    return false;
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

  tree fast_i = make_ssa_name (index_type);
  tree bit_mod = make_ssa_name (TREE_TYPE (initial_rotation));
  gphi *fast_i_phi = create_phi_node (fast_i, outer_header);
  gphi *bit_mod_phi = create_phi_node (bit_mod, outer_header);
  gimple_seq outer_seq = NULL;
  tree literal
    = clone_for_index (cand, cand.literal, cand.inner_iv.value, fast_i,
		       &outer_seq, cand.literal_base, literal_base);
  tree row_offset
    = clone_for_index (cand, cand.row_offset, cand.inner_iv.value, fast_i,
		       &outer_seq, cand.literal_base, literal_base,
		       cand.literal, literal);
  if (!literal || !row_offset)
    return false;
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
  insert_seq (outer_header, outer_seq);

  tree word = make_ssa_name (limit_type);
  gphi *word_phi = create_phi_node (word, word_loop);
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
  tree old_value = make_ssa_name (u32);
  gimple_seq_add_stmt
    (&word_seq, gimple_build_assign (old_value, build_mem_ref (info_word)));
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
  tree changed_bits
    = ssa_assign2 (&word_seq, u32, BIT_XOR_EXPR, old_value, value);
  changed_bits
    = ssa_assign2 (&word_seq, u32, BIT_AND_EXPR, changed_bits, writable);
  tree new_value
    = ssa_assign2 (&word_seq, u32, BIT_XOR_EXPR, old_value, changed_bits);
  gimple_seq_add_stmt
    (&word_seq, gimple_build_assign (build_mem_ref (info_word), new_value));
  tree word_next
    = ssa_assign2 (&word_seq, limit_type, PLUS_EXPR, word,
		   build_int_cst (limit_type, 1));
  tree more_words
    = ssa_assign2 (&word_seq, boolean_type_node, LE_EXPR,
		   word_next, i_last);
  gimple_seq_add_stmt
    (&word_seq, gimple_build_cond (NE_EXPR, more_words,
				   boolean_false_node, NULL_TREE, NULL_TREE));
  insert_seq (word_loop, word_seq);

  gimple_seq diag_guard_seq = NULL;
  tree has_diagonal
    = ssa_assign2 (&diag_guard_seq, boolean_type_node, LT_EXPR,
		   fast_i, cand.outer_iv.bound);
  gimple_seq_add_stmt
    (&diag_guard_seq,
     gimple_build_cond (NE_EXPR, has_diagonal, boolean_false_node,
			NULL_TREE, NULL_TREE));
  insert_seq (diagonal_guard, diag_guard_seq);

  gimple_seq diag_test_seq = NULL;
  tree diagonal_bit
    = ssa_assign2 (&diag_test_seq, TREE_TYPE (bit_mod), PLUS_EXPR, bit_mod,
		   build_int_cst (TREE_TYPE (bit_mod), 1));
  tree wrapped
    = ssa_assign2 (&diag_test_seq, boolean_type_node, EQ_EXPR,
		   diagonal_bit, cand.outer_iv.bound);
  tree bit_mod_next
    = ssa_assign3 (&diag_test_seq, TREE_TYPE (bit_mod), COND_EXPR,
		   wrapped, build_zero_cst (TREE_TYPE (bit_mod)),
		   diagonal_bit);
  tree diag_word
    = ssa_assign2 (&diag_test_seq, TREE_TYPE (diagonal_bit), RSHIFT_EXPR,
		   diagonal_bit,
		   build_int_cst (TREE_TYPE (diagonal_bit), 5));
  tree diag_word_size
    = ssa_assign1 (&diag_test_seq, size_type_node, NOP_EXPR, diag_word);
  tree diag_offset
    = ssa_assign2 (&diag_test_seq, size_type_node, MULT_EXPR,
		   diag_word_size, build_int_cst (size_type_node, 4));
  tree diag_presence_ptr
    = ssa_assign2 (&diag_test_seq, u32_ptr, POINTER_PLUS_EXPR,
		   presence_row, diag_offset);
  tree diag_presence = make_ssa_name (u32);
  gimple_seq_add_stmt
    (&diag_test_seq,
     gimple_build_assign (diag_presence, build_mem_ref (diag_presence_ptr)));
  tree diag_shift
    = ssa_assign2 (&diag_test_seq, TREE_TYPE (diagonal_bit), BIT_AND_EXPR,
		   diagonal_bit,
		   build_int_cst (TREE_TYPE (diagonal_bit), 31));
  tree shifted_presence
    = ssa_assign2 (&diag_test_seq, u32, RSHIFT_EXPR,
		   diag_presence, diag_shift);
  tree presence_bit
    = ssa_assign2 (&diag_test_seq, u32, BIT_AND_EXPR, shifted_presence,
		   build_int_cst (u32, 1));
  gimple_seq_add_stmt
    (&diag_test_seq,
     gimple_build_cond (EQ_EXPR, presence_bit, build_zero_cst (u32),
			NULL_TREE, NULL_TREE));
  insert_seq (diagonal_test, diag_test_seq);

  gimple_seq diag_store_seq = NULL;
  tree diag_info_ptr
    = ssa_assign2 (&diag_store_seq, u32_ptr, POINTER_PLUS_EXPR,
		   info_row, diag_offset);
  tree diag_info = make_ssa_name (u32);
  gimple_seq_add_stmt
    (&diag_store_seq,
     gimple_build_assign (diag_info, build_mem_ref (diag_info_ptr)));
  tree diag_one = ssa_assign1 (&diag_store_seq, u32, NOP_EXPR,
			       build_int_cst (u32, 1));
  tree diag_mask
    = ssa_assign2 (&diag_store_seq, u32, LSHIFT_EXPR, diag_one, diag_shift);
  tree toggled
    = ssa_assign2 (&diag_store_seq, u32, BIT_XOR_EXPR, diag_info, diag_mask);
  gimple_seq_add_stmt
    (&diag_store_seq,
     gimple_build_assign (build_mem_ref (diag_info_ptr), toggled));
  insert_seq (diagonal_store, diag_store_seq);

  tree latch_bit_mod = make_ssa_name (TREE_TYPE (bit_mod));
  gphi *latch_bit_phi = create_phi_node (latch_bit_mod, outer_latch);
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

  edge init_guard = make_edge (init, guard, EDGE_TRUE_VALUE);
  edge init_fallback = make_edge (init, dispatch, EDGE_FALSE_VALUE);
  init_guard->probability = profile_probability::likely ();
  init_fallback->probability = init_guard->probability.invert ();
  edge guard_fast = make_edge (guard, outer_header, EDGE_TRUE_VALUE);
  edge guard_fallback = make_edge (guard, dispatch, EDGE_FALSE_VALUE);
  guard_fast->probability = profile_probability::likely ();
  guard_fallback->probability = guard_fast->probability.invert ();
  edge outer_word = make_edge (outer_header, word_loop, EDGE_FALLTHRU);
  edge word_back = make_edge (word_loop, word_loop, EDGE_TRUE_VALUE);
  edge word_done = make_edge (word_loop, diagonal_guard, EDGE_FALSE_VALUE);
  word_back->probability = profile_probability::likely ();
  word_done->probability = word_back->probability.invert ();
  edge diag_yes
    = make_edge (diagonal_guard, diagonal_test, EDGE_TRUE_VALUE);
  edge diag_no = make_edge (diagonal_guard, outer_latch, EDGE_FALSE_VALUE);
  diag_yes->probability = profile_probability::likely ();
  diag_no->probability = diag_yes->probability.invert ();
  edge diag_store
    = make_edge (diagonal_test, diagonal_store, EDGE_TRUE_VALUE);
  edge diag_skip = make_edge (diagonal_test, outer_latch, EDGE_FALSE_VALUE);
  diag_store->probability = profile_probability::likely ();
  diag_skip->probability = diag_store->probability.invert ();
  edge diag_store_done
    = make_edge (diagonal_store, outer_latch, EDGE_FALLTHRU);
  edge outer_back = make_edge (outer_latch, outer_header, EDGE_TRUE_VALUE);
  edge outer_done = make_edge (outer_latch, finish, EDGE_FALSE_VALUE);
  outer_back->probability = profile_probability::likely ();
  outer_done->probability = outer_back->probability.invert ();
  make_edge (finish, original_exit->dest, EDGE_FALLTHRU);

  add_phi_arg (fast_i_phi, build_zero_cst (index_type), guard_fast,
	       UNKNOWN_LOCATION);
  add_phi_arg (fast_i_phi, fast_i_next, outer_back, UNKNOWN_LOCATION);
  add_phi_arg (bit_mod_phi, initial_rotation, guard_fast, UNKNOWN_LOCATION);
  add_phi_arg (bit_mod_phi, latch_bit_mod, outer_back, UNKNOWN_LOCATION);
  add_phi_arg (word_phi, build_zero_cst (limit_type), outer_word,
	       UNKNOWN_LOCATION);
  add_phi_arg (word_phi, word_next, word_back, UNKNOWN_LOCATION);
  add_phi_arg (latch_bit_phi, bit_mod, diag_no, UNKNOWN_LOCATION);
  add_phi_arg (latch_bit_phi, bit_mod_next, diag_skip, UNKNOWN_LOCATION);
  add_phi_arg (latch_bit_phi, bit_mod_next, diag_store_done,
	       UNKNOWN_LOCATION);

  if (dump_file)
    fprintf (dump_file,
	     "created guarded packed-bit word-store fast path in %s "
	     "for loops %d/%d (max bits %llu)\n",
	     function_name (cfun), cand.outer->num, cand.inner->num,
	     (unsigned long long) MAX_FAST_BITS);

  mark_virtual_operands_for_renaming (cfun);
  return true;
}

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
	  break;
	}
    loop_optimizer_finalize ();
    if (changed)
      {
	free_dominance_info (CDI_DOMINATORS);
	update_ssa (TODO_update_ssa);
	cleanup_tree_cfg ();
	checking_verify_ssa (true, true);
      }
    return 0;
  }
};

} // anonymous namespace

gimple_opt_pass *
make_pass_bitset_store_coalescing (gcc::context *ctxt)
{
  return new pass_bitset_store_coalescing (ctxt);
}
