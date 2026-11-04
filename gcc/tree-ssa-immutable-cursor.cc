/* Bounded path specialization for cursors into immutable objects.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pass.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "cfghooks.h"
#include "tree-inline.h"
#include "tree-ssa-threadupdate.h"
#include "tree-ssa-alias.h"
#include "options.h"

namespace {

/* These are per-function accounting, reset by the enclosing optional pass.
   Count the full copied cost, before subsequent scalar passes remove any
   statements.  Do not assume that object size bounds the loop trip count.  */
unsigned cursor_growth_left;
unsigned cursor_analysis_left;
bool cursor_changed;

struct cursor_value
{
  tree name;
  tree value;
};

class cursor_path;
static cursor_path *active_cursor_path;
static tree cursor_valueize (tree);

class cursor_path
{
public:
  auto_vec<cursor_value, 64> values;
  auto_vec<edge, 16> edges;
  auto_vec<edge, 16> best;
  unsigned best_score = 0;
  unsigned best_cost = 0;
  unsigned resolving = 0;

  tree valueize (tree op)
  {
    if (TREE_CODE (op) != SSA_NAME)
      return op;
    for (unsigned i = values.length (); i; --i)
      if (values[i - 1].name == op)
	return values[i - 1].value ? values[i - 1].value : op;

    /* Follow only acyclic assignment definitions, and retain only literal
       constants.  PHIs are interpreted from the actual path edge below.
       In particular, never infer a value for a mutable memory object from
       a store encountered on the path.  */
    if (resolving >= 12 || !cursor_analysis_left
	|| SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
      return op;
    gimple *def = SSA_NAME_DEF_STMT (op);
    if (!is_gimple_assign (def) || gimple_has_volatile_ops (def))
      return op;
    --cursor_analysis_left;
    ++resolving;
    tree value = gimple_fold_stmt_to_constant_1 (def, cursor_valueize,
					       cursor_valueize);
    --resolving;
    return value && is_gimple_min_invariant (value) ? value : op;
  }

  bool immutable_address_p (tree value)
  {
    value = valueize (value);
    if (TREE_CODE (value) != ADDR_EXPR)
      return false;
    tree base = TREE_OPERAND (value, 0);
    while (handled_component_p (base))
      base = TREE_OPERAND (base, 0);
    if (TREE_CODE (base) == MEM_REF)
      {
	/* This is an address expression, not a load of a pointer.  */
	if (resolving >= 12)
	  return false;
	++resolving;
	bool result = immutable_address_p (TREE_OPERAND (base, 0));
	--resolving;
	return result;
      }
    if (TREE_CODE (base) == STRING_CST)
      return true;
    return TREE_CODE (base) == VAR_DECL && TREE_READONLY (base)
      && !TREE_THIS_VOLATILE (base) && !DECL_EXTERNAL (base)
      && !DECL_WEAK (base) && DECL_INITIAL (base)
      && TREE_CONSTANT (DECL_INITIAL (base))
      && TREE_TYPE (base) && TYPE_SIZE_UNIT (TREE_TYPE (base))
      && tree_fits_uhwi_p (TYPE_SIZE_UNIT (TREE_TYPE (base)));
  }

  bool seed_p (edge entry)
  {
    if (entry->flags & (EDGE_ABNORMAL | EDGE_EH))
      return false;
    for (gphi_iterator gpi = gsi_start_phis (entry->dest);
	 !gsi_end_p (gpi); gsi_next (&gpi))
      {
	gphi *phi = gpi.phi ();
	tree lhs = gimple_phi_result (phi);
	if (POINTER_TYPE_P (TREE_TYPE (lhs))
	    && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs)
	    && immutable_address_p (PHI_ARG_DEF_FROM_EDGE (phi, entry)))
	  return true;
      }
    return false;
  }

  void remember (unsigned score, unsigned cost)
  {
    if (edges.length () < 2 || !score
	|| score < best_score
	|| score == best_score)
      return;
    best.truncate (0);
    for (edge e : edges)
      best.safe_push (e);
    best_score = score;
    best_cost = MAX (cost, 1U);
  }

  /* Explore a path, preserving all off-path successors in the generic
     thread copier.  An unknown condition is never replaced by a guessed
     direction.  Only the final, proved condition may be removed.  */
  void visit (edge entry, unsigned cost, unsigned score,
	      unsigned uncertain, unsigned depth)
  {
    basic_block bb = entry->dest;
    if (!cursor_analysis_left || depth > 12
	|| bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
	|| entry->flags & (EDGE_ABNORMAL | EDGE_EH)
	|| !can_duplicate_block_p (bb))
      return;
    /* The generic path copier does not duplicate nested loop regions.
       Retain the best prefix, rather than queuing a path it will reject.  */
    if (!edges.is_empty ()
	&& bb->loop_father != edges[0]->dest->loop_father)
      return;
    for (edge e : edges)
      if (e->dest == bb)
	return;
    edge e;
    edge_iterator ei;
    FOR_EACH_EDGE (e, ei, bb->succs)
      if (e->flags & (EDGE_ABNORMAL | EDGE_EH))
	return;

    const unsigned saved_values = values.length ();
    const unsigned saved_edges = edges.length ();
    edges.safe_push (entry);
    auto_vec<cursor_value, 8> phis;
    for (gphi_iterator gpi = gsi_start_phis (bb);
	 !gsi_end_p (gpi); gsi_next (&gpi))
      {
	gphi *phi = gpi.phi ();
	tree lhs = gimple_phi_result (phi);
	if (virtual_operand_p (lhs))
	  continue;
	tree value = valueize (PHI_ARG_DEF_FROM_EDGE (phi, entry));
	phis.safe_push ({ lhs, is_gimple_min_invariant (value)
			       ? value : NULL_TREE });
      }
    /* All PHI arguments observe the state before any result is assigned.  */
    for (cursor_value value : phis)
      values.safe_push (value);

    gimple *last = NULL;
    tree condition = NULL_TREE;
    bool within_budget = true;
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (is_gimple_debug (stmt) || gimple_code (stmt) == GIMPLE_LABEL)
	  continue;
	if (!cursor_analysis_left)
	  { within_budget = false; break; }
	--cursor_analysis_left;
	cost += estimate_num_insns (stmt, &eni_size_weights);
	if (cost > (unsigned) param_immutable_cursor_max_path_insns
	    || cost > cursor_growth_left)
	  { within_budget = false; break; }
	last = stmt;
	tree value = NULL_TREE;
	if (!gimple_has_volatile_ops (stmt)
	    && (is_gimple_assign (stmt) || is_gimple_call (stmt)
		|| gimple_code (stmt) == GIMPLE_COND))
	  value = gimple_fold_stmt_to_constant_1 (stmt, cursor_valueize,
						cursor_valueize);
	else if (gimple_code (stmt) == GIMPLE_SWITCH)
	  value = valueize (gimple_switch_index (as_a<gswitch *> (stmt)));

	if (gimple_code (stmt) == GIMPLE_COND
	    || gimple_code (stmt) == GIMPLE_SWITCH)
	  condition = value;
	else if (tree lhs = gimple_get_lhs (stmt))
	  if (TREE_CODE (lhs) == SSA_NAME)
	    values.safe_push ({ lhs, value && is_gimple_min_invariant (value)
				     ? value : NULL_TREE });
      }

    if (within_budget)
      {
	if (condition && TREE_CODE (condition) == INTEGER_CST)
	  {
	    if (edge taken = find_taken_edge (bb, condition))
	      {
		edges.safe_push (taken);
		remember (score + 1, cost);
		edges.pop ();
		visit (taken, cost, score + 1, uncertain, depth + 1);
	      }
	  }
	else if (single_succ_p (bb))
	  {
	    edge next = single_succ_edge (bb);
	    visit (next, cost, score, uncertain, depth + 1);
	  }
	else if (last && uncertain < 1 && gimple_code (last) == GIMPLE_COND)
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    visit (e, cost, score, uncertain + 1, depth + 1);
      }
    values.truncate (saved_values);
    edges.truncate (saved_edges);
  }
};

static tree
cursor_valueize (tree op)
{
  return active_cursor_path->valueize (op);
}

const pass_data cursor_data = {
  GIMPLE_PASS, "immutable-cursor", OPTGROUP_LOOP, TV_TREE_SSA_THREAD_JUMPS,
  PROP_cfg | PROP_ssa, 0, 0, 0, 0
};

class pass_immutable_cursor_specialization : public gimple_opt_pass
{
public:
  pass_immutable_cursor_specialization (gcc::context *ctxt)
    : gimple_opt_pass (cursor_data, ctxt) {}
  bool gate (function *) final override
  {
    if (!flag_tree_immutable_cursor_specialization || !flag_thread_jumps
	|| optimize < 2
	|| !param_immutable_cursor_growth_budget)
      return false;
    cursor_analysis_left = 2048;
    cursor_path path;
    active_cursor_path = &path;
    bool found = false;
    basic_block bb;
    FOR_EACH_BB_FN (bb, cfun)
      {
	edge e;
	edge_iterator ei;
	FOR_EACH_EDGE (e, ei, bb->preds)
	  if (path.seed_p (e))
	    { found = true; break; }
	if (found)
	  break;
      }
    active_cursor_path = NULL;
    return found;
  }
  unsigned execute (function *) final override
  {
    cursor_growth_left = param_immutable_cursor_growth_budget;
    cursor_analysis_left = 32768;
    cursor_changed = false;
    return 0;
  }
};

const pass_data cursor_thread_data = {
  GIMPLE_PASS, "cursor-thread", OPTGROUP_LOOP, TV_TREE_SSA_THREAD_JUMPS,
  PROP_cfg | PROP_ssa, 0, 0, 0, TODO_update_ssa
};

class pass_immutable_cursor_thread : public gimple_opt_pass
{
public:
  pass_immutable_cursor_thread (gcc::context *ctxt)
    : gimple_opt_pass (cursor_thread_data, ctxt) {}
  opt_pass *clone () final override
  { return new pass_immutable_cursor_thread (m_ctxt); }
  bool gate (function *) final override
  { return cursor_growth_left && cursor_analysis_left; }
  unsigned execute (function *fun) final override
  {
    loop_optimizer_init (LOOPS_HAVE_PREHEADERS | LOOPS_HAVE_SIMPLE_LATCHES);
    back_jt_path_registry registry;
    basic_block bb;
    unsigned paths = 0;
    FOR_EACH_BB_FN (bb, fun)
      {
	if (!cursor_growth_left || !cursor_analysis_left || paths == 32)
	  break;
	if (gsi_end_p (gsi_start_phis (bb)))
	  continue;
	edge entry;
	edge_iterator ei;
	FOR_EACH_EDGE (entry, ei, bb->preds)
	  {
	    cursor_path path;
	    active_cursor_path = &path;
	    if (!path.seed_p (entry))
	      continue;
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Immutable cursor seed on edge %d -> %d.\n",
		       entry->src->index, entry->dest->index);
	    path.visit (entry, 0, 0, 0, 0);
	    if (path.best.is_empty ())
	      continue;
	    vec<jump_thread_edge *> *thread = registry.allocate_thread_path ();
	    for (unsigned i = 0; i < path.best.length (); ++i)
	      registry.push_edge (thread, path.best[i],
				  i + 1 == path.best.length ()
				  ? EDGE_NO_COPY_SRC_BLOCK
				  : EDGE_COPY_SRC_BLOCK);
	    if (registry.register_jump_thread (thread))
	      {
		cursor_growth_left -= path.best_cost;
		++paths;
		if (dump_file)
		  fprintf (dump_file, "Immutable cursor path: %u proved branches, "
			   "%u copied insns, %u edges.\n", path.best_score,
			   path.best_cost, path.best.length ());
	      }
	    if (!cursor_growth_left || !cursor_analysis_left || paths == 32)
	      break;
	  }
      }
    active_cursor_path = NULL;
    cursor_changed = registry.thread_through_all_blocks (true);
    if (!cursor_changed)
      cursor_analysis_left = 0;
    if (dump_file)
      fprintf (dump_file, "Immutable cursor CFG changed: %s.\n",
	       cursor_changed ? "yes" : "no");
    loop_optimizer_finalize ();
    return cursor_changed ? TODO_cleanup_cfg : 0;
  }
};

/* Keep optional scalar pass identities distinct from the ordinary pipeline,
   including when the enclosing option is disabled.  */
enum cursor_scalar_kind { CURSOR_CCP, CURSOR_FORWPROP, CURSOR_DCE, CURSOR_FRE };

static gimple_opt_pass *
cursor_scalar (gcc::context *ctxt, cursor_scalar_kind kind)
{
  gimple_opt_pass *pass;
  switch (kind)
    {
    case CURSOR_CCP:
      pass = make_pass_ccp (ctxt);
      pass->set_pass_param (0, true);
      return pass;
    case CURSOR_FORWPROP: return make_pass_forwprop (ctxt);
    case CURSOR_DCE: return make_pass_dce (ctxt);
    case CURSOR_FRE:
      pass = make_pass_fre (ctxt);
      pass->set_pass_param (0, true);
      return pass;
    }
  gcc_unreachable ();
}

static pass_data
cursor_scalar_data (gimple_opt_pass *inner, cursor_scalar_kind kind)
{
  static const char *const names[]
    = { "cursor-ccp", "cursor-forwprop", "cursor-dce", "cursor-fre" };
  pass_data data = *inner;
  data.name = names[kind];
  return data;
}

class pass_cursor_scalar : public gimple_opt_pass
{
  gimple_opt_pass *m_inner;
  cursor_scalar_kind m_kind;
public:
  pass_cursor_scalar (gcc::context *ctxt, cursor_scalar_kind kind,
		      gimple_opt_pass *inner)
    : gimple_opt_pass (cursor_scalar_data (inner, kind), ctxt),
      m_inner (inner), m_kind (kind) {}
  ~pass_cursor_scalar () { delete m_inner; }
  opt_pass *clone () final override
  { return new pass_cursor_scalar (m_ctxt, m_kind,
				  cursor_scalar (m_ctxt, m_kind)); }
  bool gate (function *fun) final override
  { return cursor_changed && m_inner->gate (fun); }
  unsigned execute (function *fun) final override
  { return m_inner->execute (fun); }
};

} // anon namespace

gimple_opt_pass *
make_pass_immutable_cursor_specialization (gcc::context *ctxt)
{ return new pass_immutable_cursor_specialization (ctxt); }
gimple_opt_pass *
make_pass_immutable_cursor_thread (gcc::context *ctxt)
{ return new pass_immutable_cursor_thread (ctxt); }

#define CURSOR_SCALAR_FACTORY(NAME, KIND) \
gimple_opt_pass *make_pass_cursor_##NAME (gcc::context *ctxt) \
{ return new pass_cursor_scalar (ctxt, KIND, cursor_scalar (ctxt, KIND)); }
CURSOR_SCALAR_FACTORY (ccp, CURSOR_CCP)
CURSOR_SCALAR_FACTORY (forwprop, CURSOR_FORWPROP)
CURSOR_SCALAR_FACTORY (dce, CURSOR_DCE)
CURSOR_SCALAR_FACTORY (fre, CURSOR_FRE)
#undef CURSOR_SCALAR_FACTORY
