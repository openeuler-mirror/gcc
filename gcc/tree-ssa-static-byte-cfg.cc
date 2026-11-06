/* Generic bounded residual specialization of immutable byte programs.
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

/* All facts come from actual GIMPLE.  Parameter hypotheses are enforced by
   a runtime guard, while immutable-object and existing-edge facts need no
   additional parameter assumption.  The original CFG provides fallback.  */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "context.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pass.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "predict.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "cfghooks.h"
#include "tree-inline.h"
#include "tree-ssa-alias.h"
#include "tree-ssa-operands.h"
#include "gimple-pretty-print.h"
#include "tree-pretty-print.h"
#include "dominance.h"
#include "cgraph.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "value-range.h"
#include "tree-vrp.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "builtins.h"
#include "tree-eh.h"
#include "options.h"
#include <vector>
#include <map>
#include <set>
#include <algorithm>

namespace
{
#define enabled flag_tree_static_byte_cfg
/* Fixed planner policy; execute does not expose additional suboptions.  */
constexpr bool pointer_key = true, byte_facts = true;
/* This is deliberately a compiler-process allowance shared by functions.  */
unsigned emission_cost = 0;
constexpr unsigned max_states = 512;
const unsigned max_insns = 1048576, max_analysis = 1048576;
using Env = std::map<unsigned, tree>;
using Names = std::set<unsigned>;
/* The folding callback is used synchronously with the current environment.
   It is cleared when planning/emission finishes; no tree pass is reentered.  */
static Env *active;

static tree
val (tree t)
{
  if (t && TREE_CODE (t) == SSA_NAME)
    {
      auto i = active->find (SSA_NAME_VERSION (t));
      if (i != active->end ())
	return i->second;
    }
  return t;
}

static bool
constant (tree t)
{
  return t && is_gimple_min_invariant (t);
}

static bool
same_env (const Env &a, const Env &b)
{
  if (a.size () != b.size ())
    return false;
  auto j = b.begin ();
  for (auto i = a.begin (); i != a.end (); ++i, ++j)
    if (i->first != j->first || !operand_equal_p (i->second, j->second, 0))
      return false;
  return true;
}

static void
assign (Env &e, tree lhs, tree rhs)
{
  if (!lhs || TREE_CODE (lhs) != SSA_NAME || virtual_operand_p (lhs))
    return;
  /* Canonicalize a folded pointer addition exactly as GCC's own
     POINTER_PLUS_EXPR case in gimple_fold_stmt_to_constant_1 does.  */
  if (rhs && POINTER_TYPE_P (TREE_TYPE (rhs)) && !constant (rhs))
    {
      tree type = TREE_TYPE (rhs), raw = rhs;
      STRIP_NOPS (raw);
      if (TREE_CODE (raw) == POINTER_PLUS_EXPR)
	{
	  tree base = TREE_OPERAND (raw, 0), offset = TREE_OPERAND (raw, 1);
	  if (TREE_CODE (base) == ADDR_EXPR && TREE_CODE (offset) == INTEGER_CST
	      && useless_type_conversion_p (type, TREE_TYPE (base)))
	    {
	      tree reference = fold_build2 (
		  MEM_REF, TREE_TYPE (TREE_TYPE (base)), unshare_expr (base),
		  fold_convert (ptr_type_node, offset));
	      rhs = build1 (ADDR_EXPR, type, reference);
	    }
	}
    }
  if (constant (rhs) && types_compatible_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
    e[SSA_NAME_VERSION (lhs)] = rhs;
  else
    e.erase (SSA_NAME_VERSION (lhs));
}

static bool
immutable (tree t)
{
  if (!t || TREE_CODE (t) != ADDR_EXPR)
    return false;
  tree b = TREE_OPERAND (t, 0);
  while (handled_component_p (b))
    b = TREE_OPERAND (b, 0);
  if (TREE_CODE (b) == MEM_REF)
    return immutable (TREE_OPERAND (b, 0));
  return TREE_CODE (b) == STRING_CST
	 || (VAR_P (b) && TREE_READONLY (b) && !TREE_THIS_VOLATILE (b)
	     && !DECL_EXTERNAL (b) && !DECL_WEAK (b) && DECL_INITIAL (b)
	     && TREE_CONSTANT (DECL_INITIAL (b)));
}

struct Arc
{
  edge original;
  edge phi_edge;
  unsigned target;
  std::vector<tree> phi_values;
};

struct State
{
  basic_block original, copy;
  Env input;
  std::vector<Arc> arcs;
  tree condition;
};

class Planner
{
public:
  std::vector<State> states;
  std::map<int, Names> live, uses, defs;
  tree guard_name = nullptr, guard_value = nullptr;

  Planner (tree name = nullptr, tree value = nullptr)
      : guard_name (name), guard_value (value)
  {
  }

  edge entry_final_edge = nullptr;
  unsigned transparent_skips = 0, constant_defs_skipped = 0,
	   lookahead_stops = 0;
  std::vector<tree> entry_phi_values;
  unsigned cost = 0, analysis = 0, reused = 0, folded = 0;
  bool failed = false, seed = false;
  Names static_names, pattern_pointers;
  const char *reason = "none";

  void
  mark_control (tree t, Names &visited, unsigned depth)
  {
    if (!t || TREE_CODE (t) != SSA_NAME || depth > 64
	|| !INTEGRAL_TYPE_P (TREE_TYPE (t)))
      return;
    unsigned v = SSA_NAME_VERSION (t);
    if (!visited.insert (v).second)
      return;
    gimple *d = SSA_NAME_DEF_STMT (t);
    if (gimple_code (d) == GIMPLE_PHI)
      {
	static_names.insert (v);
	for (unsigned i = 0; i < gimple_phi_num_args (d); ++i)
	  mark_control (gimple_phi_arg_def (d, i), visited, depth + 1);
      }
    else if (is_gimple_assign (d) && gimple_assign_single_p (d) == false)
      {
	static_names.insert (v);
	for (unsigned i = 1; i < gimple_num_ops (d); ++i)
	  mark_control (gimple_op (d, i), visited, depth + 1);
      }
  }

  void
  binding_times ()
  {
    basic_block cb;
    Names visited;
    FOR_EACH_BB_FN (cb, cfun)
      {
	gimple_stmt_iterator cg = gsi_last_bb (cb);
	gimple *t = gsi_end_p (cg) ? nullptr : gsi_stmt (cg);
	if (t && gimple_code (t) == GIMPLE_COND)
	  {
	    tree a = gimple_cond_lhs (t), b = gimple_cond_rhs (t);
	    if (TREE_CODE (a) == INTEGER_CST)
	      mark_control (b, visited, 0);
	    if (TREE_CODE (b) == INTEGER_CST)
	      mark_control (a, visited, 0);
	  }
      }
    bool changed = true;
    unsigned rounds = 0;
    while (changed && ++rounds < 4096)
      {
	changed = false;
	basic_block b;
	FOR_EACH_BB_FN (b, cfun)
	  {
	    for (gphi_iterator g = gsi_start_phis (b); !gsi_end_p (g);
		 gsi_next (&g))
	      {
		gphi *p = g.phi ();
		bool derived = false;
		for (unsigned i = 0; i < gimple_phi_num_args (p); ++i)
		  {
		    tree a = gimple_phi_arg_def (p, i);
		    derived
			|= immutable (a)
			   || (TREE_CODE (a) == SSA_NAME
			       && static_names.count (SSA_NAME_VERSION (a)));
		  }
		tree lhs = gimple_phi_result (p);
		if (derived && !virtual_operand_p (lhs))
		  changed
		      |= static_names.insert (SSA_NAME_VERSION (lhs)).second;
	      }
	    for (gimple_stmt_iterator g = gsi_start_bb (b); !gsi_end_p (g);
		 gsi_next (&g))
	      {
		gimple *s = gsi_stmt (g);
		tree lhs = gimple_get_lhs (s);
		if (!lhs || TREE_CODE (lhs) != SSA_NAME
		    || virtual_operand_p (lhs))
		  continue;
		bool derived = false;
		ssa_op_iter it;
		tree a;
		FOR_EACH_SSA_TREE_OPERAND (a, s, it, SSA_OP_USE)
		  derived |= static_names.count (SSA_NAME_VERSION (a));
		if (is_gimple_assign (s))
		  for (unsigned i = 1; i < gimple_num_ops (s); ++i)
		    derived |= immutable (gimple_op (s, i));
		if (derived)
		  changed
		      |= static_names.insert (SSA_NAME_VERSION (lhs)).second;
	      }
	  }
      }
    if (changed)
      {
	failed = true;
	reason = "binding-fixpoint";
      }
    changed = true;
    rounds = 0;
    while (changed && ++rounds < 4096)
      {
	changed = false;
	basic_block b;
	FOR_EACH_BB_FN (b, cfun)
	  {
	    for (gphi_iterator g = gsi_start_phis (b); !gsi_end_p (g);
		 gsi_next (&g))
	      {
		gphi *p = g.phi ();
		tree lhs = gimple_phi_result (p);
		if (!POINTER_TYPE_P (TREE_TYPE (lhs)))
		  continue;
		bool derived = false;
		for (unsigned i = 0; i < gimple_phi_num_args (p); ++i)
		  {
		    tree a = gimple_phi_arg_def (p, i);
		    derived |= immutable (a)
			       || (TREE_CODE (a) == SSA_NAME
				   && pattern_pointers.count (
				       SSA_NAME_VERSION (a)));
		  }
		if (derived)
		  changed |= pattern_pointers.insert (SSA_NAME_VERSION (lhs))
				 .second;
	      }
	    for (gimple_stmt_iterator g = gsi_start_bb (b); !gsi_end_p (g);
		 gsi_next (&g))
	      {
		gimple *t = gsi_stmt (g);
		if (!is_gimple_assign (t))
		  continue;
		tree lhs = gimple_assign_lhs (t);
		if (TREE_CODE (lhs) != SSA_NAME
		    || !POINTER_TYPE_P (TREE_TYPE (lhs)))
		  continue;
		tree a = gimple_assign_rhs1 (t);
		enum tree_code code = gimple_assign_rhs_code (t);
		if ((code == SSA_NAME || code == ADDR_EXPR
		     || code == POINTER_PLUS_EXPR || CONVERT_EXPR_CODE_P (code))
		    && (immutable (a)
			|| (TREE_CODE (a) == SSA_NAME
			    && pattern_pointers.count (SSA_NAME_VERSION (a)))))
		  changed |= pattern_pointers.insert (SSA_NAME_VERSION (lhs))
				 .second;
	      }
	  }
      }
    if (changed)
      {
	failed = true;
	reason = "pointer-provenance-fixpoint";
      }
  }

  void
  liveness ()
  {
    basic_block b;
    FOR_EACH_BB_FN (b, cfun)
      {
	Names seen;
	for (gimple_stmt_iterator g = gsi_start_bb (b); !gsi_end_p (g);
	     gsi_next (&g))
	  {
	    gimple *s = gsi_stmt (g);
	    ssa_op_iter it;
	    tree op;
	    FOR_EACH_SSA_TREE_OPERAND (op, s, it, SSA_OP_USE)
	      if (!virtual_operand_p (op)
		  && !seen.count (SSA_NAME_VERSION (op)))
		uses[b->index].insert (SSA_NAME_VERSION (op));
	    FOR_EACH_SSA_TREE_OPERAND (op, s, it, SSA_OP_DEF)
	      if (!virtual_operand_p (op))
		seen.insert (SSA_NAME_VERSION (op));
	  }
	defs[b->index] = seen;
	live[b->index] = uses[b->index];
      }
    bool changed = true;
    unsigned rounds = 0;
    while (changed && ++rounds <= 256)
      {
	changed = false;
	FOR_EACH_BB_FN (b, cfun)
	  {
	    Names out;
	    edge e;
	    edge_iterator ei;
	    FOR_EACH_EDGE (e, ei, b->succs)
	      {
		const Names required = live[e->dest->index];
		Names n = required, incoming;
		/* PHI operands are parallel assignments. Never remap a name
		   inserted for an earlier PHI as if it were a later PHI result.
		 */
		for (gphi_iterator g = gsi_start_phis (e->dest); !gsi_end_p (g);
		     gsi_next (&g))
		  {
		    gphi *p = g.phi ();
		    tree r = gimple_phi_result (p);
		    if (virtual_operand_p (r))
		      continue;
		    /* All original PHIs are copied, including semantically dead
		       ones. Keep every incoming SSA operand structurally
		       available.  */
		    n.erase (SSA_NAME_VERSION (r));
		    tree a = PHI_ARG_DEF_FROM_EDGE (p, e);
		    if (TREE_CODE (a) == SSA_NAME)
		      incoming.insert (SSA_NAME_VERSION (a));
		  }
		n.insert (incoming.begin (), incoming.end ());
		out.insert (n.begin (), n.end ());
	      }
	    for (unsigned n : defs[b->index])
	      out.erase (n);
	    out.insert (uses[b->index].begin (), uses[b->index].end ());
	    if (out != live[b->index])
	      {
		live[b->index] = out;
		changed = true;
	      }
	  }
      }
    if (changed)
      failed = true;
  }

  bool
  charge ()
  {
    if (++analysis > max_analysis)
      {
	failed = true;
	reason = "analysis-cost-budget";
	return false;
      }
    return true;
  }

  bool
  keepable (tree lhs, tree v)
  {
    if (!lhs || TREE_CODE (lhs) != SSA_NAME || virtual_operand_p (lhs)
	|| !constant (v)
	|| !useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (v)))
      return false;
    bool narrow = INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		  && TYPE_PRECISION (TREE_TYPE (lhs)) <= 8;
    return narrow || immutable (v) || (guard_name && lhs == guard_name);
  }

  edge
  contract (edge e, Env &env, bool single_path)
  {
    unsigned steps = 0;
    active = &env;
    while (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
      {
	/* Bounded interpretation may revisit a loop with different constants.
	   On exhaustion, materialize this real destination BEFORE its PHIs.  */
	if (steps++ >= max_states)
	  {
	    ++lookahead_stops;
	    break;
	  }
	basic_block bb = e->dest;
	if (!charge ())
	  break;
	if (!can_duplicate_block_p (bb))
	  break;
	bool bad = false;
	edge ie;
	edge_iterator ii;
	FOR_EACH_EDGE (ie, ii, bb->preds)
	  if (ie->flags
	      & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL | EDGE_EH | EDGE_FAKE))
	    bad = true;
	if (bad)
	  break;
	Env trial = env;
	active = &trial;
	std::vector<std::pair<tree, tree> > phis;
	unsigned definitions = 0;
	for (gphi_iterator gi = gsi_start_phis (bb); !gsi_end_p (gi);
	     gsi_next (&gi))
	  {
	    if (!charge ())
	      {
		bad = true;
		break;
	      }
	    tree lhs = gimple_phi_result (gi.phi ()),
		 v = val (PHI_ARG_DEF_FROM_EDGE (gi.phi (), e));
	    if (!keepable (lhs, v))
	      {
		bad = true;
		break;
	      }
	    phis.push_back ({ lhs, v });
	  }
	if (!bad)
	  for (auto p : phis)
	    {
	      assign (trial, p.first, p.second);
	      ++definitions;
	    }
	gimple *cond = nullptr;
	for (gimple_stmt_iterator gi = gsi_start_bb (bb);
	     !bad && !gsi_end_p (gi); gsi_next (&gi))
	  {
	    gimple *t = gsi_stmt (gi);
	    if (is_gimple_debug (t))
	      continue;
	    if (!charge ())
	      {
		bad = true;
		break;
	      }
	    if (gimple_has_volatile_ops (t) || gimple_vdef (t)
		|| stmt_could_throw_p (cfun, t))
	      {
		bad = true;
		break;
	      }
	    if (gimple_code (t) == GIMPLE_COND && !cond)
	      {
		tree type = TREE_TYPE (gimple_cond_lhs (t));
		if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
		  {
		    bad = true;
		    break;
		  }
		cond = t;
	      }
	    else if (is_gimple_assign (t) && !cond)
	      {
		tree lhs = gimple_assign_lhs (t);
		if (TREE_CODE (lhs) != SSA_NAME
		    || (!INTEGRAL_TYPE_P (TREE_TYPE (lhs))
			&& !POINTER_TYPE_P (TREE_TYPE (lhs))))
		  {
		    bad = true;
		    break;
		  }
		for (unsigned i = 1; i < gimple_num_ops (t); ++i)
		  {
		    tree op = gimple_op (t, i);
		    if (op && TREE_TYPE (op)
			&& !INTEGRAL_TYPE_P (TREE_TYPE (op))
			&& !POINTER_TYPE_P (TREE_TYPE (op)))
		      bad = true;
		  }
		if (bad)
		  break;
		if (gimple_assign_rhs_code (t) == MEM_REF
		    && !immutable (
			val (TREE_OPERAND (gimple_assign_rhs1 (t), 0))))
		  {
		    bad = true;
		    break;
		  }
		tree v = gimple_fold_stmt_to_constant_1 (t, val, val);
		assign (trial, lhs, v);
		if (!keepable (lhs, val (lhs)))
		  {
		    bad = true;
		    break;
		  }
		++definitions;
	      }
	    else
	      {
		bad = true;
		break;
	      }
	  }
	edge next = nullptr;
	if (!bad && cond)
	  {
	    tree v = gimple_fold_stmt_to_constant_1 (cond, val, val);
	    if (v && TREE_CODE (v) == INTEGER_CST)
	      next = find_taken_edge (bb, v);
	  }
	else if (!bad && single_succ_p (bb))
	  next = single_succ_edge (bb);
	if (bad || !next
	    || next->flags
		   & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL | EDGE_EH | EDGE_FAKE)
	    || next->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
	    || (!single_path && EDGE_COUNT (next->dest->preds) > 1))
	  {
	    active = &env;
	    break;
	  }
	/* Commit only after the complete block and its outgoing path are
	 * proved.  */
	env.swap (trial);
	active = &env;
	++transparent_skips;
	constant_defs_skipped += definitions;
	if (cond)
	  ++folded;
	e = next;
      }
    active = &env;
    return e;
  }

  unsigned
  intern (edge e, Env env, edge *resolved = nullptr, bool single_path = false,
	  std::vector<tree> *phi_values = nullptr)
  {
    e = contract (e, env, single_path);
    if (resolved)
      *resolved = e;
    if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
      return ~0U;
    active = &env;
    std::vector<std::pair<tree, tree> > phis;
    for (gphi_iterator g = gsi_start_phis (e->dest); !gsi_end_p (g);
	 gsi_next (&g))
      phis.push_back ({ gimple_phi_result (g.phi ()),
			val (PHI_ARG_DEF_FROM_EDGE (g.phi (), e)) });
    if (phi_values)
      {
	phi_values->clear ();
	for (auto p : phis)
	  phi_values->push_back (p.second);
      }
    for (auto p : phis)
      assign (env, p.first, p.second);
    const Names &needed = live[e->dest->index];
    for (auto i = env.begin (); i != env.end ();)
      {
	bool narrow = byte_facts
		      && INTEGRAL_TYPE_P (TREE_TYPE (ssa_name (i->first)))
		      && TYPE_PRECISION (TREE_TYPE (ssa_name (i->first))) <= 8;
	bool guarded = guard_name && i->first == SSA_NAME_VERSION (guard_name);
	if (!needed.count (i->first)
	    || (!static_names.count (i->first) && !narrow && !guarded
		&& !immutable (i->second))
	    || (pointer_key && !immutable (i->second) && !narrow && !guarded))
	  i = env.erase (i);
	else
	  ++i;
      }
    for (unsigned i = 0; i < states.size (); ++i)
      if (states[i].original == e->dest && same_env (states[i].input, env))
	{
	  ++reused;
	  return i;
	}
    if (states.size () >= max_states)
      {
	std::map<unsigned, std::vector<tree> > values;
	for (const State &s : states)
	  if (s.original == e->dest)
	    for (auto p : s.input)
	      {
		auto &vs = values[p.first];
		bool found = false;
		for (tree v : vs)
		  found |= operand_equal_p (v, p.second, 0);
		if (!found)
		  vs.push_back (p.second);
	      }
	for (auto &p : values)
	  if (p.second.size () > 4)
	    {
	      if (dump_file)
		fprintf (dump_file, "KEY-GROWTH bb=%d ssa=%u values=%zu def=",
			 e->dest->index, p.first, p.second.size ());
	      if (dump_file)
		print_gimple_stmt (dump_file,
				   SSA_NAME_DEF_STMT (ssa_name (p.first)), 0,
				   TDF_SLIM);
	      for (unsigned i = 0;
		   i < std::min (8U, (unsigned)p.second.size ()); ++i)
		{
		  if (dump_file)
		    print_generic_expr (dump_file, p.second[i], TDF_SLIM);
		  if (dump_file)
		    fprintf (dump_file, ";");
		}
	      if (dump_file)
		fprintf (dump_file, "\n");
	    }
	failed = true;
	reason = "state-budget";
	return ~0U;
      }
    states.push_back ({ e->dest, nullptr, env, {}, nullptr });
    return states.size () - 1;
  }

  bool
  plan ()
  {
    if (!single_succ_p (ENTRY_BLOCK_PTR_FOR_FN (cfun)))
      {
	if (dump_file)
	  fprintf (dump_file, "RESIDUAL-REJECT %s blocks=%u entry-or-size\n",
		   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)),
		   n_basic_blocks_for_fn (cfun));
	return false;
      }
    basic_block b;
    FOR_EACH_BB_FN (b, cfun)
      {
	for (gphi_iterator g = gsi_start_phis (b); !gsi_end_p (g);
	     gsi_next (&g))
	  for (unsigned i = 0; i < gimple_phi_num_args (g.phi ()); ++i)
	    seed |= immutable (gimple_phi_arg_def (g.phi (), i));
	for (gimple_stmt_iterator g = gsi_start_bb (b); !gsi_end_p (g);
	     gsi_next (&g))
	  {
	    gimple *s = gsi_stmt (g);
	    if (is_gimple_debug (s) || gimple_code (s) == GIMPLE_LABEL)
	      continue;
	    if (is_gimple_assign (s))
	      for (unsigned i = 1; i < gimple_num_ops (s); ++i)
		seed |= immutable (gimple_op (s, i));
	  }
      }
    if (!seed)
      {
	reason = "no-immutable-seed";
	return false;
      }
    binding_times ();
    liveness ();
    if (failed)
      return false;
    Env initial;
    if (guard_name)
      assign (initial, guard_name, guard_value);
    intern (single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)), initial,
	    &entry_final_edge, true, &entry_phi_values);
    for (unsigned k = 0; k < states.size () && !failed; ++k)
      {
	basic_block bb = states[k].original;
	Env env = states[k].input;
	active = &env;
	tree cond = nullptr;
	gimple *last = nullptr;
	if (!can_duplicate_block_p (bb))
	  {
	    failed = true;
	    reason = "nonduplicable-block";
	    break;
	  }
	for (gimple_stmt_iterator g = gsi_start_bb (bb); !gsi_end_p (g);
	     gsi_next (&g))
	  {
	    gimple *s = gsi_stmt (g);
	    if (is_gimple_debug (s) || gimple_code (s) == GIMPLE_LABEL)
	      continue;
	    tree v = gimple_fold_stmt_to_constant_1 (s, val, val);
	    if (is_gimple_assign (s) && gimple_assign_rhs_code (s) == MEM_REF)
	      {
		tree ptr = TREE_OPERAND (gimple_assign_rhs1 (s), 0),
		     pv = val (ptr);
		bool tracked
		    = TREE_CODE (ptr) == SSA_NAME
		      && pattern_pointers.count (SSA_NAME_VERSION (ptr));
		if ((immutable (pv) || tracked) && !constant (v))
		  {
		    if (dump_file)
		      fprintf (dump_file, "LOAD-REJECT bb=%d stmt=", bb->index);
		    if (dump_file)
		      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
		    if (dump_file)
		      fprintf (dump_file, "pointer=");
		    if (dump_file)
		      print_generic_expr (dump_file, pv, TDF_SLIM);
		    if (dump_file)
		      fprintf (dump_file, "\n");
		    failed = true;
		    reason = "unfoldable-immutable-load-or-bounds";
		    break;
		  }
	      }
	    bool known_call = false;
	    if (is_gimple_call (s))
	      {
		/* Retain the original call and its VUSE at exactly this dynamic
		   point. This permits residual dynamic results; no user callee
		   is interpreted.  */
		known_call
		    = (gimple_call_flags (s) & (ECF_CONST | ECF_PURE)) != 0;
	      }
	    /* Only the residual paths must be free of memory writes.  Fallback
	     */
	    /* blocks are retained.  Copied loads and pure calls keep their
	       VUSEs; duplicate_block registers virtual PHI definitions for
	       update_ssa.  */
	    if (gimple_has_volatile_ops (s) || gimple_vdef (s)
		|| (is_gimple_call (s) && !known_call)
		|| stmt_could_throw_p (cfun, s)
		|| (gimple_code (s) != GIMPLE_ASSIGN
		    && gimple_code (s) != GIMPLE_COND
		    && gimple_code (s) != GIMPLE_RETURN && !known_call))
	      {
		if (dump_file)
		  fprintf (
		      dump_file, "SPLITPLAN-REJECT %s state=%u bb=%d stmt=",
		      IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)), k,
		      bb->index);
		if (dump_file)
		  print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
		failed = true;
		reason = "effects-eh-or-unsupported";
		break;
	      }

	    if (++analysis > max_analysis
		|| (cost += estimate_num_insns (s, &eni_size_weights))
		       > max_insns)
	      {
		failed = true;
		reason = "analysis-cost-budget";
		break;
	      }
	    last = s;
	    if (gimple_code (s) == GIMPLE_COND)
	      cond = v;
	    else
	      assign (env, gimple_get_lhs (s), v);
	  }
	if (failed)
	  break;
	edge selected = (cond && TREE_CODE (cond) == INTEGER_CST)
			    ? find_taken_edge (bb, cond)
			    : nullptr;
	states[k].condition = selected ? cond : nullptr;
	if (selected)
	  ++folded;
	std::vector<Arc> arcs;
	edge e;
	edge_iterator ei;
	FOR_EACH_EDGE (e, ei, bb->succs)
	  {
	    if (selected && e != selected)
	      continue;
	    if (e->flags & (EDGE_ABNORMAL | EDGE_EH))
	      {
		failed = true;
		reason = "abnormal-eh-edge";
		break;
	      }
	    Env next = env;
	    active = &env;
	    /* Bounded byte equality facts describe only this existing edge.
	       No branch is discarded unless GCC folding proves its condition.
	     */
	    if (byte_facts && last && gimple_code (last) == GIMPLE_COND)
	      {
		enum tree_code code = gimple_cond_code (last);
		bool eq = (code == EQ_EXPR && (e->flags & EDGE_TRUE_VALUE))
			  || (code == NE_EXPR && (e->flags & EDGE_FALSE_VALUE));
		if (eq)
		  {
		    tree a = gimple_cond_lhs (last), b = gimple_cond_rhs (last);
		    tree av = val (a), bv = val (b);
		    if (TREE_CODE (a) == SSA_NAME
			&& TREE_CODE (bv) == INTEGER_CST)
		      assign (next, a, bv);
		    if (TREE_CODE (b) == SSA_NAME
			&& TREE_CODE (av) == INTEGER_CST)
		      assign (next, b, av);
		  }
	      }
	    edge phi_edge;
	    std::vector<tree> pv;
	    unsigned target = intern (e, next, &phi_edge,
				      selected || single_succ_p (bb), &pv);
	    arcs.push_back ({ e, phi_edge, target, pv });
	  }
	states[k].arcs = arcs;
      }
    active = nullptr;
    if (!failed)
      reason = folded ? "complete" : "no-folds";
    return !failed && folded;
  }

  void
  emit ()
  {
    gcc_assert (!states.empty ());
    std::vector<basic_block> originals;
    basic_block ob;
    FOR_EACH_BB_FN (ob, cfun)
      originals.push_back (ob);
    initialize_original_copy_tables ();
    for (State &s : states)
      s.copy = duplicate_block (s.original, nullptr, nullptr);
    /* Residual state execution frequencies are unknown for every emission.  */
    for (State &s : states)
      s.copy->count = profile_count::uninitialized ();
    if (guard_name)
      for (basic_block b : originals)
	b->count = b->count.apply_probability (profile_probability::even ());
    edge entry = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
    basic_block oldstart = entry->dest;
    std::vector<std::pair<tree, location_t> > initial, fast_initial;
    unsigned fi = 0;
    for (gphi_iterator g = gsi_start_phis (states[0].original); !gsi_end_p (g);
	 gsi_next (&g))
      fast_initial.push_back (
	  { entry_phi_values[fi++],
	    gimple_phi_arg_location_from_edge (g.phi (), entry_final_edge) });
    for (gphi_iterator g = gsi_start_phis (oldstart); !gsi_end_p (g);
	 gsi_next (&g))
      initial.push_back (
	  { PHI_ARG_DEF_FROM_EDGE (g.phi (), entry),
	    gimple_phi_arg_location_from_edge (g.phi (), entry) });
    for (State &s : states)
      {
	Env env = s.input;
	active = &env;
	/* Removed constant definitions have no corresponding debug SSA
	   producer. Conservatively drop copied debug statements rather than
	   retain stale binds.  */
	for (gimple_stmt_iterator dg = gsi_start_bb (s.copy); !gsi_end_p (dg);)
	  {
	    if (is_gimple_debug (gsi_stmt (dg)))
	      gsi_remove (&dg, true);
	    else
	      gsi_next (&dg);
	  }
	for (gimple_stmt_iterator g = gsi_start_bb (s.copy); !gsi_end_p (g);
	     gsi_next (&g))
	  {
	    gimple *t = gsi_stmt (g);
	    if (is_gimple_debug (t) || gimple_code (t) == GIMPLE_LABEL)
	      continue;
	    gcc_checking_assert (!gimple_vdef (t));
	    ssa_op_iter it;
	    use_operand_p u;
	    FOR_EACH_SSA_USE_OPERAND (u, t, it, SSA_OP_USE)
	      {
		tree v = val (USE_FROM_PTR (u));
		if (constant (v)
		    && useless_type_conversion_p (TREE_TYPE (USE_FROM_PTR (u)),
						  TREE_TYPE (v)))
		  SET_USE (u, unshare_expr (v));
	      }
	    /* GCC's propagation canonicalizer flattens
	       MEM[&MEM[base,off],off2]. A min invariant address alone is not a
	       legal MEM_REF base.  */
	    fold_stmt_inplace (&g);
	    update_stmt (t);
	  }
	if (s.condition)
	  {
	    gimple_stmt_iterator g = gsi_last_bb (s.copy);
	    gcc_assert (gimple_code (gsi_stmt (g)) == GIMPLE_COND);
	    gsi_remove (&g, true);
	  }
	std::vector<edge> edges;
	edge e;
	edge_iterator ei;
	FOR_EACH_EDGE (e, ei, s.copy->succs)
	  edges.push_back (e);
	for (edge ce : edges)
	  {
	    Arc *a = nullptr;
	    for (Arc &ar : s.arcs)
	      if (ar.original->dest == ce->dest
		  && ar.original->flags == ce->flags)
		{
		  a = &ar;
		  break;
		}
	    if (!a)
	      {
		remove_edge (ce);
		continue;
	      }
	    if (s.condition)
	      {
		ce->flags = EDGE_FALLTHRU;
		ce->probability = profile_probability::always ();
	      }
	    if (a->target == ~0U)
	      continue;
	    State &dest = states[a->target];
	    redirect_edge_succ (ce, dest.copy);
	    unsigned pi = 0;
	    gphi_iterator og = gsi_start_phis (dest.original),
			  cg = gsi_start_phis (dest.copy);
	    /* Dynamic inputs remain per-edge SSA uses.  add_phi_arg maintains
	     */
	    /* immediate uses, and the registered old/new definitions let the
	       SSA updater select the reaching definition in each copied path.
	     */
	    for (; !gsi_end_p (og); gsi_next (&og), gsi_next (&cg))
	      add_phi_arg (
		  cg.phi (), a->phi_values[pi++], ce,
		  gimple_phi_arg_location_from_edge (og.phi (), a->phi_edge));
	  }
      }
    active = nullptr;
    basic_block guard = create_empty_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun));
    if (current_loops)
      add_bb_to_loop (guard, current_loops->tree_root);
    guard->count = entry->count ();
    redirect_edge_succ (entry, guard);
    gimple_stmt_iterator gi = gsi_start_bb (guard);
    gsi_insert_after (
	&gi,
	gimple_build_cond (EQ_EXPR, guard_name ? guard_name : integer_one_node,
			   guard_value ? guard_value : integer_one_node,
			   nullptr, nullptr),
	GSI_NEW_STMT);
    edge yes = make_edge (guard, states[0].copy, EDGE_TRUE_VALUE),
	 no = make_edge (guard, oldstart, EDGE_FALSE_VALUE);
    /* With no measured profile, split a real hypothesis evenly.  A plan  */
    /* using only immutable and existing-edge facts has an unconditional arm. */
    yes->probability = guard_name ? profile_probability::even ()
				  : profile_probability::always ();
    no->probability = guard_name ? profile_probability::even ()
				 : profile_probability::never ();
    unsigned i = 0;
    for (gphi_iterator g = gsi_start_phis (oldstart); !gsi_end_p (g);
	 gsi_next (&g), ++i)
      add_phi_arg (g.phi (), initial[i].first, no, initial[i].second);
    i = 0;
    for (gphi_iterator g = gsi_start_phis (states[0].copy); !gsi_end_p (g);
	 gsi_next (&g), ++i)
      add_phi_arg (g.phi (), fast_initial[i].first, yes,
		   fast_initial[i].second);
    if (current_loops)
      loops_state_set (LOOPS_NEED_FIXUP);
    free_dominance_info (CDI_DOMINATORS);
    free_dominance_info (CDI_POST_DOMINATORS);
    update_ssa (TODO_update_ssa);
    free_original_copy_tables ();
    rebuild_frequencies ();
  }
};

const pass_data data = { GIMPLE_PASS,
			 "static-byte-cfg",
			 OPTGROUP_LOOP,
			 TV_TREE_SSA_OTHER,
			 PROP_cfg | PROP_ssa,
			 0,
			 0,
			 0,
			 0 };

struct Pass : gimple_opt_pass
{
  Pass (gcc::context *c) : gimple_opt_pass (data, c)
  {
  }

  bool
  gate (function *) override
  {
    return enabled && optimize >= 2 && !cfun->can_throw_non_call_exceptions
	   && !flag_non_call_exceptions;
  }

  unsigned
  execute (function *) override
  {
    cgraph_node *n = cgraph_node::get (cfun->decl);
    if (!n)
      return 0;
    if (n_basic_blocks_for_fn (cfun) > 256)
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file,
		   "RESIDUAL-REJECT %s blocks=%u function-size-budget\n",
		   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)),
		   n_basic_blocks_for_fn (cfun));
	return 0;
      }
    std::vector<std::pair<tree, tree> > candidates = { { nullptr, nullptr } };
    basic_block bb;
    FOR_EACH_BB_FN (bb, cfun)
      {
	for (gimple_stmt_iterator gi = gsi_start_bb (bb); !gsi_end_p (gi);
	     gsi_next (&gi))
	  {
	    gimple *stmt = gsi_stmt (gi);
	    if (!is_gimple_assign (stmt)
		|| gimple_assign_rhs_code (stmt) != POINTER_PLUS_EXPR)
	      continue;
	    tree base = gimple_assign_rhs1 (stmt),
		 index = gimple_assign_rhs2 (stmt);
	    if (TREE_CODE (base) != ADDR_EXPR
		|| TREE_CODE (TREE_OPERAND (base, 0)) != STRING_CST
		|| TREE_CODE (index) != SSA_NAME
		|| !SSA_NAME_IS_DEFAULT_DEF (index) || !SSA_NAME_VAR (index)
		|| TREE_CODE (SSA_NAME_VAR (index)) != PARM_DECL
		|| !INTEGRAL_TYPE_P (TREE_TYPE (index))
		|| !TYPE_UNSIGNED (TREE_TYPE (index)))
	      continue;
	    bool current_parameter = false;
	    for (tree parm = DECL_ARGUMENTS (cfun->decl); parm;
		 parm = DECL_CHAIN (parm))
	      current_parameter |= parm == SSA_NAME_VAR (index);
	    if (!current_parameter)
	      continue;
	    tree literal = TREE_OPERAND (base, 0),
		 array_type = TREE_TYPE (literal);
	    if (TREE_CODE (array_type) != ARRAY_TYPE
		|| !INTEGRAL_TYPE_P (TREE_TYPE (array_type))
		|| TYPE_PRECISION (TREE_TYPE (array_type)) != 8)
	      continue;
	    unsigned length = TREE_STRING_LENGTH (literal);
	    if (length <= 1 || length > 256
		|| TYPE_PRECISION (TREE_TYPE (index)) < 8)
	      continue;
	    tree value = build_int_cst (TREE_TYPE (index), length - 1);
	    bool duplicate = false;
	    for (auto c : candidates)
	      if (c.first == index && c.second
		  && operand_equal_p (c.second, value, 0))
		duplicate = true;
	    if (!duplicate && candidates.size () < 3)
	      candidates.push_back ({ index, value });
	  }
      }
    for (auto candidate : candidates)
      {
	Planner p (candidate.first, candidate.second);
	bool ok = p.plan ();
	if (dump_file)
	  fprintf (dump_file,
		   "GUARDPLAN %s guarded=%d states=%zu cost=%u folded=%u "
		   "status=%s reason=%s\n",
		   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)),
		   candidate.first != nullptr, p.states.size (), p.cost,
		   p.folded, ok ? "ready" : "rejected", p.reason);
	if (p.transparent_skips)
	  if (dump_file)
	    fprintf (dump_file, "TRANSPARENT-SKIP %s count=%u\n",
		     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)),
		     p.transparent_skips);
	if (p.constant_defs_skipped)
	  if (dump_file)
	    fprintf (dump_file, "CONSTANT-DEFS-SKIP %s count=%u\n",
		     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)),
		     p.constant_defs_skipped);
	if (p.lookahead_stops)
	  if (dump_file)
	    fprintf (dump_file, "CONSTANT-LOOKAHEAD-STOP %s count=%u\n",
		     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)),
		     p.lookahead_stops);
	if (ok && p.cost <= 4096 && emission_cost + p.cost <= 32768)
	  {
	    if (dump_file)
	      fprintf (dump_file, "RESIDUAL-EMIT %s cost=%u\n",
		       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)),
		       p.cost);
	    if (candidate.first)
	      {
		if (dump_file)
		  fprintf (dump_file, "GUARDED-CONTEXT ");
		if (dump_file)
		  print_generic_expr (dump_file, candidate.first, TDF_SLIM);
		if (dump_file)
		  fprintf (dump_file, " == ");
		if (dump_file)
		  print_generic_expr (dump_file, candidate.second, TDF_SLIM);
		if (dump_file)
		  fprintf (dump_file, "\n");
	      }
	    emission_cost += p.cost;
	    p.emit ();
	    return TODO_cleanup_cfg | TODO_update_ssa
		   | TODO_rebuild_cgraph_edges | TODO_verify_all;
	  }
	if (ok && dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file,
		   "RESIDUAL-REJECT %s emission-budget cost=%u used=%u\n",
		   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)),
		   p.cost, emission_cost);
      }
    return 0;
  }
};

}

gimple_opt_pass *
make_pass_static_byte_cfg (gcc::context *ctxt)
{
  return new Pass (ctxt);
}
