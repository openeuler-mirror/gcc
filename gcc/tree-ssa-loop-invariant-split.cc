/* Loop invariant split.
   Copyright (C) 2026 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-ssa-loop-manip.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "cfghooks.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop-ivopts.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "tm_p.h"
#include "tree-data-ref.h"
#include "tree-chrec.h"
#include "tree-vectorizer.h"
#include "alias.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "tree-inline.h"
#include "cfgexpand.h"
#include "dumpfile.h"

/* This pass performs a deliberately conservative first form of loop nest
   splitting.  For a strict two-level nest

      for (i)
	{
	  acc = init (i);
	  for (j = 0; j < N; ++j)
	    acc = f (acc, invariant_load (j), other_loads (i, j));
	  store (i, acc);
	}

   it creates a sequence of outer-loop copies, each with the inner loop
   restricted to a chunk of the original j iteration space.  Later chunks
   seed supported accumulator PHIs by loading the partial value stored by the
   previous chunk.  The intended profitability hook is to reduce the amount of
   j-only data that a later vectorizer/unroller/LIM phase sees in each outer
   loop copy.

   The legality checks intentionally reject anything outside this simple
   pattern.  In particular, memory dependences must either be proven absent or
   be the exact zero-distance dependences that naturally occur between accesses
   in the same original iteration.  */

namespace {

struct split_chunk
{
  unsigned HOST_WIDE_INT low;
  unsigned HOST_WIDE_INT high;
};

struct inner_control
{
  gphi *iv_phi;
  gcond *exit_cond;
  tree init;
  tree next;
  tree bound;
  enum tree_code code;
};

struct carry_info
{
  gphi *phi;
  unsigned phi_index;
  tree store_ref;
};

struct split_plan
{
  class loop *outer;
  class loop *inner;
  auto_vec<split_chunk, 8> chunks;
  auto_vec<carry_info, 1> carries;
  unsigned HOST_WIDE_INT chunk_iters;
  unsigned HOST_WIDE_INT chunk_bytes;
  unsigned HOST_WIDE_INT bytes_per_inner_iter;
};

static int transformed_loop_marker;

static dump_user_location_t
linvsplit_location (class loop *loop)
{
  return find_loop_location (loop);
}

static void
linvsplit_missed (class loop *loop, const char *msg)
{
  if (dump_enabled_p ())
    dump_printf_loc (MSG_MISSED_OPTIMIZATION, linvsplit_location (loop),
		     "loop invariant split: %s\n", msg);
}

static void
linvsplit_missed (gimple *stmt, const char *msg)
{
  if (dump_enabled_p ())
    dump_printf_loc (MSG_MISSED_OPTIMIZATION, stmt,
		     "loop invariant split: %s\n", msg);
}

static bool
tree_to_uhwi_checked (tree t, unsigned HOST_WIDE_INT *value)
{
  if (!t || TREE_CODE (t) != INTEGER_CST || !tree_fits_uhwi_p (t))
    return false;
  *value = tree_to_uhwi (t);
  return true;
}

static bool
constant_inner_trip_count (class loop *inner, unsigned HOST_WIDE_INT *trip)
{
  edge exit = single_exit (inner);
  class tree_niter_desc niter;

  if (!exit
      || !number_of_iterations_exit (inner, exit, &niter, false, true)
      || !integer_zerop (niter.may_be_zero))
    return false;

  unsigned HOST_WIDE_INT latch_iters;
  if (!tree_to_uhwi_checked (niter.niter, &latch_iters))
    return false;

  *trip = latch_iters + 1;
  return *trip > 0;
}

static bool
match_inner_control (class loop *inner, unsigned HOST_WIDE_INT trip,
		     inner_control *control)
{
  edge entry = loop_preheader_edge (inner);
  edge latch = loop_latch_edge (inner);
  edge exit = single_exit (inner);
  gcond *cond = get_loop_exit_condition (inner);

  if (!entry || !latch || !exit || !cond || gimple_bb (cond) != exit->src)
    return false;

  for (gphi_iterator psi = gsi_start_phis (inner->header);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree result = PHI_RESULT (phi);
      if (!INTEGRAL_TYPE_P (TREE_TYPE (result)))
	continue;

      tree init = PHI_ARG_DEF_FROM_EDGE (phi, entry);
      tree next = PHI_ARG_DEF_FROM_EDGE (phi, latch);
      affine_iv iv;
      if (!simple_iv (inner, inner, result, &iv, false)
	  || !integer_onep (iv.step)
	  || !operand_equal_p (iv.base, init, 0))
	continue;

      tree lhs = gimple_cond_lhs (cond);
      tree rhs = gimple_cond_rhs (cond);
      if (lhs != next)
	continue;

      unsigned HOST_WIDE_INT initv, boundv;
      if (!tree_to_uhwi_checked (init, &initv)
	  || !tree_to_uhwi_checked (rhs, &boundv)
	  || initv != 0
	  || boundv != trip)
	continue;

      switch (gimple_cond_code (cond))
	{
	case NE_EXPR:
	case LT_EXPR:
	  control->iv_phi = phi;
	  control->exit_cond = cond;
	  control->init = init;
	  control->next = next;
	  control->bound = rhs;
	  control->code = gimple_cond_code (cond);
	  return true;
	default:
	  continue;
	}
    }

  return false;
}

static bool
strict_two_level_nest_p (class loop *outer, class loop **inner)
{
  if (!outer->inner || outer->inner->next || outer->inner->inner)
    return false;

  *inner = outer->inner;
  return (loop_outer (*inner) == outer
	  && single_exit (outer)
	  && single_exit (*inner)
	  && loop_preheader_edge (outer)
	  && loop_preheader_edge (*inner)
	  && loop_latch_edge (outer)
	  && loop_latch_edge (*inner));
}

static bool
stmt_side_effects_forbidden_p (gimple *stmt)
{
  if (gimple_has_volatile_ops (stmt))
    return true;

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      return true;
    case GIMPLE_ASM:
      return true;
    case GIMPLE_RESX:
    case GIMPLE_EH_DISPATCH:
    case GIMPLE_TRANSACTION:
      return true;
    default:
      return false;
    }
}

static bool
copyable_loop_nest_p (class loop *outer)
{
  basic_block *bbs = get_loop_body (outer);
  bool ok = can_copy_bbs_p (bbs, outer->num_nodes);

  for (unsigned i = 0; ok && i < outer->num_nodes; ++i)
    {
      basic_block bb = bbs[i];
      if (bb->flags & BB_IRREDUCIBLE_LOOP)
	{
	  ok = false;
	  break;
	}
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (stmt_side_effects_forbidden_p (stmt))
	    {
	      ok = false;
	      break;
	    }
	}
    }

  free (bbs);
  return ok;
}

/* Return true if EXPR's scalar evolution has a non-zero affine step in LOOP.
   This helper treats unanalyzable expressions conservatively as varying.  */

static bool
scev_varies_in_loop_p (tree expr, class loop *loop)
{
  if (!expr)
    return true;

  tree scev = analyze_scalar_evolution (loop, expr);
  scev = instantiate_parameters (loop, scev);
  if (!scev || chrec_contains_undetermined (scev))
    return true;

  tree step = evolution_part_in_loop_num (scev, loop->num);
  return step && !integer_zerop (step);
}

static tree
single_lcssa_phi_result (class loop *loop, tree value)
{
  edge exit = single_exit (loop);
  if (!exit)
    return NULL_TREE;

  for (gphi_iterator psi = gsi_start_phis (exit->dest);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      if (PHI_ARG_DEF_FROM_EDGE (phi, exit) == value)
	return PHI_RESULT (phi);
    }

  return NULL_TREE;
}

static unsigned HOST_WIDE_INT
ref_size_bytes (tree ref)
{
  tree type = TREE_TYPE (ref);
  if (!type || !TYPE_SIZE_UNIT (type) || !tree_fits_uhwi_p (TYPE_SIZE_UNIT (type)))
    return 0;
  return tree_to_uhwi (TYPE_SIZE_UNIT (type));
}

static bool
candidate_invariant_load_p (data_reference_p dr, class loop *outer,
			    class loop *inner,
			    unsigned HOST_WIDE_INT *bytes)
{
  if (!DR_IS_READ (dr)
      || DR_IS_CONDITIONAL_IN_STMT (dr)
      || TREE_THIS_VOLATILE (DR_REF (dr)))
    return false;

  unsigned HOST_WIDE_INT size = ref_size_bytes (DR_REF (dr));
  if (!size)
    return false;

  if (!DR_STEP (dr) || !tree_fits_shwi_p (DR_STEP (dr)))
    return false;
  if (tree_to_shwi (DR_STEP (dr)) != (HOST_WIDE_INT) size)
    return false;

  for (unsigned i = 0; i < DR_NUM_DIMENSIONS (dr); ++i)
    {
      tree access_fn = DR_ACCESS_FN (dr, i);
      if (chrec_contains_undetermined (access_fn))
	return false;

      tree inner_step = evolution_part_in_loop_num (access_fn, inner->num);
      if (inner_step && !integer_zerop (inner_step))
	continue;

      if (scev_varies_in_loop_p (access_fn, outer))
	return false;
    }

  if (!expr_invariant_in_loop_p (outer, DR_BASE_ADDRESS (dr))
      || !expr_invariant_in_loop_p (outer, DR_OFFSET (dr))
      || !expr_invariant_in_loop_p (outer, DR_INIT (dr)))
    return false;

  *bytes = size;
  return true;
}

static bool
collect_candidate_loads (class loop *outer, class loop *inner,
			 vec<data_reference_p> *datarefs,
			 unsigned HOST_WIDE_INT *bytes_per_iter)
{
  *bytes_per_iter = 0;
  bool found = false;

  for (data_reference_p dr : *datarefs)
    {
      if (!flow_bb_inside_loop_p (inner, gimple_bb (DR_STMT (dr))))
	continue;

      unsigned HOST_WIDE_INT bytes = 0;
      if (!candidate_invariant_load_p (dr, outer, inner, &bytes))
	continue;

      found = true;
      *bytes_per_iter += bytes;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "loop invariant split candidate load: ");
	  print_generic_expr (dump_file, DR_REF (dr), TDF_SLIM);
	  fprintf (dump_file, " (%llu bytes)\n",
		   (unsigned long long) bytes);
	}
    }

  return found;
}

static bool
stmt_writes_ref_p (gimple *stmt, tree ref)
{
  if (!stmt || gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  gassign *assign = as_a<gassign *> (stmt);
  return (gimple_store_p (assign)
	  && operand_equal_p (gimple_assign_lhs (assign), ref, 0));
}

static bool
carry_store_dependence_p (ddr_p ddr, const split_plan *plan)
{
  if (!DR_IS_WRITE (DDR_A (ddr)) || !DR_IS_WRITE (DDR_B (ddr)))
    return false;

  for (const carry_info &carry : plan->carries)
    if (stmt_writes_ref_p (DR_STMT (DDR_A (ddr)), carry.store_ref)
	&& stmt_writes_ref_p (DR_STMT (DDR_B (ddr)), carry.store_ref))
      return true;

  return false;
}

static bool
safe_data_dependences_p (class loop *outer, vec<ddr_p> *ddrs,
			 const split_plan *plan)
{
  for (ddr_p ddr : *ddrs)
    {
      if (DR_IS_READ (DDR_A (ddr)) && DR_IS_READ (DDR_B (ddr)))
	continue;

      if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
	continue;

      if (carry_store_dependence_p (ddr, plan))
	continue;

      if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know
	  || DDR_NUM_DIST_VECTS (ddr) != 1)
	{
	  linvsplit_missed (outer, "alias or dependence analysis is unknown");
	  return false;
	}

      lambda_vector dist = DDR_DIST_VECT (ddr, 0);
      if (!lambda_vector_zerop (dist, DDR_NB_LOOPS (ddr))
	  || DR_IS_WRITE (DDR_A (ddr))
	  || DR_IS_WRITE (DDR_B (ddr)))
	{
	  linvsplit_missed (outer, "loop-carried memory dependence");
	  return false;
	}
    }

  return true;
}

static gimple *
single_use_stmt_outside_loop (tree value, class loop *loop)
{
  if (TREE_CODE (value) != SSA_NAME)
    return NULL;

  use_operand_p use_p;
  imm_use_iterator iter;
  gimple *the_use = NULL;
  FOR_EACH_IMM_USE_FAST (use_p, iter, value)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;
      if (flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
	return NULL;
      if (the_use)
	return NULL;
      the_use = use_stmt;
    }

  return the_use;
}

static bool
find_carry_store_for_phi (class loop *outer, class loop *inner,
			  gphi *phi, carry_info *carry)
{
  tree latch_value = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (inner));
  tree live_out = single_lcssa_phi_result (inner, latch_value);
  if (!live_out)
    return false;

  gimple *use_stmt = single_use_stmt_outside_loop (live_out, inner);

  if (!use_stmt || gimple_code (use_stmt) != GIMPLE_ASSIGN)
    return false;

  gassign *assign = as_a<gassign *> (use_stmt);
  if (gimple_assign_single_p (assign)
      && gimple_store_p (assign)
      && gimple_assign_rhs1 (assign) == live_out)
    {
      tree ref = gimple_assign_lhs (assign);
      if (expr_invariant_in_loop_p (inner, ref)
	  && !expr_invariant_in_loop_p (outer, ref))
	{
	  carry->phi = phi;
	  carry->store_ref = ref;
	  return true;
	}
    }

  return false;
}

static bool
collect_carries (class loop *outer, class loop *inner, inner_control *control,
		 split_plan *plan)
{
  edge entry = loop_preheader_edge (inner);
  edge latch = loop_latch_edge (inner);
  unsigned phi_index = 0;

  for (gphi_iterator psi = gsi_start_phis (inner->header);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      unsigned this_phi_index = phi_index++;
      if (virtual_operand_p (PHI_RESULT (phi)) || phi == control->iv_phi)
	continue;

      tree back = PHI_ARG_DEF_FROM_EDGE (phi, latch);
      if (back == PHI_ARG_DEF_FROM_EDGE (phi, entry))
	continue;

      carry_info carry;
      if (!find_carry_store_for_phi (outer, inner, phi, &carry))
	{
	  linvsplit_missed (phi, "no carry store for inner live-out value");
	  return false;
	}
      carry.phi_index = this_phi_index;
      plan->carries.safe_push (carry);
    }

  return true;
}

static bool
target_vector_budget (class loop *loop, unsigned HOST_WIDE_INT *budget,
		      unsigned HOST_WIDE_INT *vector_bytes,
		      unsigned *vector_regs)
{
  if (param_loop_invariant_split_chunk_bytes != 0)
    {
      *budget = param_loop_invariant_split_chunk_bytes;
      *vector_bytes = 0;
      *vector_regs = 0;
      return true;
    }

  machine_mode vector_mode = targetm.vectorize.preferred_simd_mode (QImode);
  if (!VECTOR_MODE_P (vector_mode)
      || !GET_MODE_SIZE (vector_mode).is_constant (vector_bytes))
    {
      linvsplit_missed (loop, "no fixed-size preferred SIMD mode");
      return false;
    }

  if (*vector_bytes == 0)
    {
      linvsplit_missed (loop, "preferred SIMD mode has zero size");
      return false;
    }

  if (param_loop_invariant_split_vector_regs != 0)
    *vector_regs = param_loop_invariant_split_vector_regs;
  else
    {
      unsigned count = 0;
      for (unsigned regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	if (!fixed_regs[regno]
	    && !global_regs[regno]
	    /* Count only the actual SIMD/FP register file.  Some targets
	       let general-purpose registers hold the same mode too, but we
	       do not want those to inflate the vector register budget.  */
	    && !reg_class_subset_p (REGNO_REG_CLASS (regno), GENERAL_REGS)
	    && targetm.hard_regno_mode_ok (regno, vector_mode))
	  ++count;
      *vector_regs = count;
    }

  if (*vector_regs == 0)
    {
      linvsplit_missed (loop, "no vector registers available");
      return false;
    }

  unsigned reserved = param_loop_invariant_split_reserved_vregs;
  if (reserved == 0)
    reserved = (*vector_regs + 1) / 2;
  if (reserved >= *vector_regs)
    {
      linvsplit_missed (loop, "all vector registers are reserved");
      return false;
    }

  *budget = (*vector_regs - reserved) * *vector_bytes;
  return true;
}

static unsigned HOST_WIDE_INT
align_chunk_iters (unsigned HOST_WIDE_INT chunk_iters,
		   unsigned HOST_WIDE_INT bytes_per_iter,
		   unsigned HOST_WIDE_INT vector_bytes)
{
  if (vector_bytes == 0 || bytes_per_iter == 0)
    return chunk_iters;

  unsigned HOST_WIDE_INT vector_iters = vector_bytes / bytes_per_iter;
  if (vector_iters <= 1)
    return chunk_iters;

  chunk_iters = (chunk_iters / vector_iters) * vector_iters;
  return chunk_iters ? chunk_iters : vector_iters;
}

static bool
compute_split_chunks (class loop *outer, unsigned HOST_WIDE_INT inner_trip,
		      unsigned HOST_WIDE_INT bytes_per_iter,
		      split_plan *plan)
{
  unsigned HOST_WIDE_INT vector_bytes = 0;
  unsigned vector_regs = 0;
  unsigned HOST_WIDE_INT budget = 0;
  if (!target_vector_budget (outer, &budget, &vector_bytes, &vector_regs))
    return false;

  if (budget < bytes_per_iter)
    {
      linvsplit_missed (outer, "chunk budget too small");
      return false;
    }

  unsigned HOST_WIDE_INT chunk_iters = budget / bytes_per_iter;
  chunk_iters = align_chunk_iters (chunk_iters, bytes_per_iter, vector_bytes);

  if (chunk_iters == 0)
    {
      linvsplit_missed (outer, "chunk iteration count is zero");
      return false;
    }
  if (chunk_iters >= inner_trip)
    {
      linvsplit_missed (outer, "chunk covers whole inner loop");
      return false;
    }

  unsigned HOST_WIDE_INT copies = CEIL (inner_trip, chunk_iters);
  if (copies > (unsigned HOST_WIDE_INT) param_loop_invariant_split_max_copies)
    {
      linvsplit_missed (outer, "max copies limit reached");
      return false;
    }

  plan->chunk_iters = chunk_iters;
  plan->chunk_bytes = chunk_iters * bytes_per_iter;
  plan->bytes_per_inner_iter = bytes_per_iter;

  for (unsigned HOST_WIDE_INT low = 0; low < inner_trip; low += chunk_iters)
    {
      split_chunk chunk;
      chunk.low = low;
      chunk.high = MIN (inner_trip, low + chunk_iters);
      plan->chunks.safe_push (chunk);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "loop invariant split target: vector_bytes=%llu, "
	       "vector_regs=%u, budget=%llu\n",
	       (unsigned long long) vector_bytes, vector_regs,
	       (unsigned long long) budget);
      for (unsigned i = 0; i < plan->chunks.length (); ++i)
	fprintf (dump_file, "loop invariant split chunk %u: [%llu, %llu)\n",
		 i,
		 (unsigned long long) plan->chunks[i].low,
		 (unsigned long long) plan->chunks[i].high);
    }

  return true;
}

static bool
analyze_loop_nest (class loop *outer, split_plan *plan)
{
  class loop *inner = NULL;
  if (!strict_two_level_nest_p (outer, &inner))
    return false;

  if (optimize_loop_for_size_p (outer) || optimize_loop_for_size_p (inner))
    {
      linvsplit_missed (outer, "loop optimized for size");
      return false;
    }

  if (!copyable_loop_nest_p (outer))
    {
      linvsplit_missed (outer, "loop nest cannot be safely copied");
      return false;
    }

  edge outer_exit = single_exit (outer);
  if (!slpeel_can_duplicate_loop_p (outer, outer_exit, outer_exit))
    {
      linvsplit_missed (outer, "loop nest is not in duplicable canonical form");
      return false;
    }

  unsigned HOST_WIDE_INT inner_trip = 0;
  if (!constant_inner_trip_count (inner, &inner_trip))
    {
      linvsplit_missed (inner, "dynamic inner bound");
      return false;
    }

  inner_control control;
  if (!match_inner_control (inner, inner_trip, &control))
    {
      linvsplit_missed (inner, "inner induction variable is not canonical");
      return false;
    }

  auto_vec<loop_p, 2> loop_nest;
  auto_vec<data_reference_p, 16> datarefs;
  auto_vec<ddr_p, 16> ddrs;
  if (!compute_data_dependences_for_loop (outer, true, &loop_nest,
					  &datarefs, &ddrs))
    {
      linvsplit_missed (outer, "data reference analysis failed");
      free_data_refs (datarefs);
      free_dependence_relations (ddrs);
      return false;
    }

  plan->outer = outer;
  plan->inner = inner;
  bool ok = collect_carries (outer, inner, &control, plan);
  if (!ok)
    {
      free_data_refs (datarefs);
      free_dependence_relations (ddrs);
      return false;
    }

  ok = safe_data_dependences_p (outer, &ddrs, plan);
  unsigned HOST_WIDE_INT bytes_per_iter = 0;
  if (ok
      && !collect_candidate_loads (outer, inner, &datarefs, &bytes_per_iter))
    {
      linvsplit_missed (inner, "no outer-invariant inner load");
      ok = false;
    }

  free_data_refs (datarefs);
  free_dependence_relations (ddrs);

  if (!ok)
    return false;

  return compute_split_chunks (outer, inner_trip, bytes_per_iter, plan);
}

static tree
build_index_cst_like (tree like, unsigned HOST_WIDE_INT value)
{
  return wide_int_to_tree (TREE_TYPE (like), value);
}

static void
replace_phi_entry_arg (gphi *phi, tree value)
{
  edge entry = loop_preheader_edge (gimple_bb (phi)->loop_father);
  SET_PHI_ARG_DEF (phi, entry->dest_idx, value);
  update_stmt (phi);
}

static void
patch_inner_loop_bounds (class loop *inner, const split_chunk &chunk)
{
  unsigned HOST_WIDE_INT trip;
  inner_control control;
  gcc_assert (constant_inner_trip_count (inner, &trip));
  gcc_assert (match_inner_control (inner, trip, &control));

  tree low = build_index_cst_like (control.init, chunk.low);
  tree high = build_index_cst_like (control.bound, chunk.high);
  replace_phi_entry_arg (control.iv_phi, low);
  gimple_cond_set_rhs (control.exit_cond, high);
  update_stmt (control.exit_cond);
}

static gphi *
find_matching_carry_phi (class loop *inner, const carry_info &carry)
{
  unsigned phi_index = 0;
  for (gphi_iterator psi = gsi_start_phis (inner->header);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      if (phi_index++ != carry.phi_index)
	continue;

      if (types_compatible_p (TREE_TYPE (PHI_RESULT (phi)),
			      TREE_TYPE (PHI_RESULT (carry.phi))))
	return phi;
      return NULL;
    }

  return NULL;
}

static tree
rematerialize_address_ssa (tree addr)
{
  if (TREE_CODE (addr) != SSA_NAME || SSA_NAME_IS_DEFAULT_DEF (addr))
    return addr;

  gimple *def = SSA_NAME_DEF_STMT (addr);
  if (!def || gimple_code (def) != GIMPLE_ASSIGN || gimple_vuse (def))
    return addr;

  return unshare_expr (gimple_assign_rhs_to_tree (def));
}

static void
insert_carry_loads (class loop *outer, class loop *inner,
		    const split_plan *plan)
{
  edge entry = loop_preheader_edge (inner);
  gimple_stmt_iterator gsi = gsi_last_bb (entry->src);

  for (const carry_info &carry : plan->carries)
    {
      gphi *phi = find_matching_carry_phi (inner, carry);
      gcc_assert (phi);

      carry_info local_carry;
      if (!find_carry_store_for_phi (outer, inner, phi, &local_carry))
	{
	  linvsplit_missed (phi, "no carry store in copied loop");
	  continue;
	}

      tree addr = build_fold_addr_expr (unshare_expr (local_carry.store_ref));
      addr = rematerialize_address_ssa (addr);
      addr = force_gimple_operand_gsi (&gsi, addr, true, NULL_TREE, false,
				       GSI_CONTINUE_LINKING);
      tree ref = build2 (MEM_REF, TREE_TYPE (local_carry.store_ref), addr,
			 build_int_cst (reference_alias_ptr_type
					  (local_carry.store_ref), 0));
      tree lhs = make_temp_ssa_name (TREE_TYPE (ref), NULL, "linvsplit_carry");
      gassign *load = gimple_build_assign (lhs, ref);
      gsi_insert_after (&gsi, load, GSI_NEW_STMT);
      replace_phi_entry_arg (phi, lhs);
    }
}

static bool
apply_split_plan (split_plan *plan)
{
  auto_vec<class loop *, 8> outer_loops;
  outer_loops.safe_push (plan->outer);

  initialize_original_copy_tables ();
  for (unsigned i = 1; i < plan->chunks.length (); ++i)
    {
      class loop *base = outer_loops[i - 1];
      edge base_exit = single_exit (base);
      class loop *copy
	= slpeel_tree_duplicate_loop_to_edge_cfg (base, base_exit, base,
						  base_exit, base_exit, NULL,
						  false /* flow_loops */);
      if (!copy)
	{
	  free_original_copy_tables ();
	  linvsplit_missed (base, "loop copy failed");
	  return false;
	}
      outer_loops.safe_push (copy);
      reset_original_copy_tables ();
    }
  free_original_copy_tables ();

  for (unsigned i = 0; i < outer_loops.length (); ++i)
    {
      class loop *outer = outer_loops[i];
      class loop *inner = outer->inner;
      outer->aux = &transformed_loop_marker;
      if (inner)
	inner->aux = &transformed_loop_marker;
      patch_inner_loop_bounds (inner, plan->chunks[i]);
      if (i != 0)
	insert_carry_loads (outer, inner, plan);
    }

  if (dump_file)
    fprintf (dump_file,
	     "loop invariant split: split inner loop into %u chunks, "
	     "chunk_iters=%llu, chunk_bytes=%llu\n",
	     plan->chunks.length (),
	     (unsigned long long) plan->chunk_iters,
	     (unsigned long long) plan->chunk_bytes);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, linvsplit_location (plan->outer),
		     "loop invariant split: split inner loop into %u chunks, "
		     "chunk_iters=%llu, chunk_bytes=%llu\n",
		     plan->chunks.length (),
		     (unsigned long long) plan->chunk_iters,
		     (unsigned long long) plan->chunk_bytes);

  return true;
}

static bool
function_has_forced_vector_loop_p (function *fun)
{
  for (auto loop : loops_list (fun, LI_FROM_INNERMOST))
    if (loop->force_vectorize)
      return true;
  return false;
}

static unsigned int
tree_ssa_loop_invariant_split (function *fun)
{
  bool changed = false;

  if (number_of_loops (fun) <= 2)
    return 0;

  if (!flag_tree_loop_vectorize && !function_has_forced_vector_loop_p (fun))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION,
			 dump_user_location_t::from_function_decl
			   (current_function_decl),
			 "loop invariant split: loop vectorization is disabled\n");
      return 0;
    }

  gcc_assert (scev_initialized_p ());
  calculate_dominance_info (CDI_DOMINATORS);

  const unsigned max_transforms = param_loop_invariant_split_max_copies;
  unsigned transforms = 0;
  bool try_again;
  do
    {
      try_again = false;
      for (auto loop : loops_list (fun, LI_FROM_INNERMOST))
	{
	  class loop *inner = NULL;
	  if (loop->aux == &transformed_loop_marker)
	    continue;
	  if (!strict_two_level_nest_p (loop, &inner))
	    continue;
	  if (inner->aux == &transformed_loop_marker)
	    continue;

	  split_plan plan;
	  if (!analyze_loop_nest (loop, &plan))
	    continue;

	  if (apply_split_plan (&plan))
	    {
	      changed = true;
	      try_again = true;
	      ++transforms;
	      scev_reset ();
	      free_numbers_of_iterations_estimates (fun);
	      break;
	    }
	}
    }
  while (try_again && transforms < max_transforms);

  for (auto loop : loops_list (fun, LI_FROM_INNERMOST))
    if (loop->aux == &transformed_loop_marker)
      loop->aux = NULL;

  if (!changed)
    return 0;

  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  return TODO_cleanup_cfg;
}

const pass_data pass_data_loop_invariant_split =
{
  GIMPLE_PASS, /* type */
  "linvsplit", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP_INVARIANT_SPLIT, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop_invariant_split : public gimple_opt_pass
{
public:
  pass_loop_invariant_split (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_loop_invariant_split, ctxt)
  {}

  bool gate (function *) final override
  {
    return flag_tree_loop_invariant_split != 0;
  }

  unsigned int execute (function *fun) final override
  {
    return tree_ssa_loop_invariant_split (fun);
  }
};

} // anon namespace

gimple_opt_pass *
make_pass_loop_invariant_split (gcc::context *ctxt)
{
  return new pass_loop_invariant_split (ctxt);
}
