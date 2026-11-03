/* IPA array load inlining for fixed lookup tables.
   Copyright (C) 2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "tree-pass.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "gimplify-me.h"
#include "fold-const.h"
#include "cgraph.h"
#include "ipa-utils.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-operands.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "hash-map.h"

extern void release_defs (gimple *);

namespace {

/* A candidate has one explicit, nonvolatile store region covering every
   element.  Its value must be a pure expression of the array indices.  For
   each load, prove that the entire region has been initialized, either
   locally or on every possible entry to the function.  A dominating dynamic
   store alone does not establish completion of an initialization loop.

   The guard fallback must account for every nonzero guard write, and prove
   that the nonzero branch edge is unavoidable on a path to the load.  Neither
   an address reference in a vtable nor a visible function body closes the
   set of possible callers.  Unknown entries or escaping guards are rejected.
   Loads and stores change only after these proofs and formula validation.  */

/* Retain function analyses until the pass finishes.  Nested proofs may revisit
   a function, so individual queries must not finalize loops or free dominance
   information owned by another query.  The rewrites do not change CFGs.  */
class function_analysis_cache
{
  struct state
  {
    function *fn;
    bool had_dominators;
    bool initialized_loops;
  };
  auto_vec<state> functions;
  hash_map<function *, unsigned> positions;

public:
  void prepare (unsigned loop_flags)
  {
    unsigned *slot = positions.get (cfun);
    unsigned pos;
    if (!slot)
      {
	pos = functions.length ();
	state entry = { cfun, dom_info_available_p (CDI_DOMINATORS), false };
	functions.safe_push (entry);
	positions.put (cfun, pos);
      }
    else
      pos = *slot;

    calculate_dominance_info (CDI_DOMINATORS);
    if (loop_flags && !functions[pos].initialized_loops)
      {
	loop_optimizer_init (loop_flags);
	functions[pos].initialized_loops = true;
      }
  }

  ~function_analysis_cache ()
  {
    for (const state &entry : functions)
      {
	push_cfun (entry.fn);
	if (entry.initialized_loops)
	  loop_optimizer_finalize ();
	if (!entry.had_dominators)
	  free_dominance_info (CDI_DOMINATORS);
	pop_cfun ();
      }
  }
};

class cfun_saver
{
public:
  cfun_saver (function_analysis_cache &analyses, cgraph_node *node,
	      unsigned loop_flags = 0)
  {
    push_cfun (DECL_STRUCT_FUNCTION (node->decl));
    analyses.prepare (loop_flags);
  }

  ~cfun_saver () { pop_cfun (); }
};

static bool
fixed_array_dims_p (tree type, auto_vec<unsigned> &dims, tree &elt_type)
{
  dims.truncate (0);
  while (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree domain = TYPE_DOMAIN (type);
      if (!domain || !TYPE_MIN_VALUE (domain) || !TYPE_MAX_VALUE (domain)
	  || TREE_CODE (TYPE_MIN_VALUE (domain)) != INTEGER_CST
	  || TREE_CODE (TYPE_MAX_VALUE (domain)) != INTEGER_CST
	  || !integer_zerop (TYPE_MIN_VALUE (domain))
	  || !tree_fits_uhwi_p (TYPE_MAX_VALUE (domain)))
	return false;

      unsigned HOST_WIDE_INT maxv = tree_to_uhwi (TYPE_MAX_VALUE (domain));
      if (maxv >= UINT_MAX)
	return false;

      dims.safe_push ((unsigned) maxv + 1);
      type = TREE_TYPE (type);
    }

  if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
    return false;

  elt_type = type;
  return dims.length () != 0;
}

static bool
extract_array_ref (tree expr, tree &decl, auto_vec<tree> &indices)
{
  auto_vec<tree, 8> rev_indices;
  tree t = expr;
  while (t && TREE_CODE (t) == ARRAY_REF)
    {
      rev_indices.safe_push (TREE_OPERAND (t, 1));
      t = TREE_OPERAND (t, 0);
    }

  if (!t || TREE_CODE (t) != VAR_DECL || rev_indices.length () == 0)
    return false;

  indices.truncate (0);
  for (int i = (int) rev_indices.length () - 1; i >= 0; --i)
    indices.safe_push (rev_indices[i]);

  decl = t;
  return true;
}

static bool
expr_contains_decl_p (tree expr, tree decl)
{
  if (!expr)
    return false;

  if (expr == decl)
    return true;

  tree_code code = TREE_CODE (expr);
  if (code == TREE_LIST)
    {
      for (tree t = expr; t; t = TREE_CHAIN (t))
	if (expr_contains_decl_p (TREE_VALUE (t), decl))
	  return true;
      return false;
    }

  int len = TREE_CODE_LENGTH (code);
  for (int i = 0; i < len; ++i)
    if (expr_contains_decl_p (TREE_OPERAND (expr, i), decl))
      return true;

  return false;
}

static void
dump_candidate (tree decl, const auto_vec<unsigned> &dims)
{
  if (!dump_file)
    return;

  unsigned HOST_WIDE_INT elements = 1;
  for (unsigned i = 0; i < dims.length (); ++i)
    elements *= dims[i];

  fprintf (dump_file, "array-load-inline: candidate array %s, dimensions ",
	   IDENTIFIER_POINTER (DECL_NAME (decl)));
  for (unsigned i = 0; i < dims.length (); ++i)
    fprintf (dump_file, "%s%u", i ? "x" : "", dims[i]);
  fprintf (dump_file, ", element count " HOST_WIDE_INT_PRINT_UNSIGNED
	   ", fixed-size array candidate\n", elements);
}

struct array_access
{
  cgraph_node *node = nullptr;
  gimple *stmt = nullptr;
  tree rhs = nullptr;
  auto_vec<tree, 4> indices;
  auto_vec<unsigned, 4> lows;
  auto_vec<unsigned, 4> highs;
  auto_vec<loop_p, 4> index_loops;
};

class array_candidate
{
public:
  explicit array_candidate (function_analysis_cache &cache)
    : analyses (cache)
  {}

  function_analysis_cache &analyses;
  hash_map<gimple *, bool> initialized_before;
  hash_map<cgraph_node *, bool> initializes_on_return;
  hash_map<gimple *, bool> load_dominance;

  varpool_node *var = nullptr;
  tree decl = nullptr;
  tree elem_type = nullptr;
  auto_vec<unsigned> dims;
  auto_vec<array_access *> stores;
  auto_vec<array_access *> loads;
  const char *reject_reason = nullptr;

  ~array_candidate ()
  {
    for (unsigned i = 0; i < stores.length (); ++i)
      delete stores[i];
    for (unsigned i = 0; i < loads.length (); ++i)
      delete loads[i];
  }

  const char *name () const
  {
    return IDENTIFIER_POINTER (DECL_NAME (decl));
  }

  bool init (varpool_node *);
  array_access *record_access (cgraph_node *, gimple *, bool, tree,
			       const auto_vec<tree> &);
};

class entry_proof_tree_node
{
public:
  explicit entry_proof_tree_node (bool enabled = true)
    : recording (enabled)
  {}

  auto_vec<cgraph_node *, 4> component;
  auto_vec<cgraph_edge *, 4> failed_caller_edges;
  cgraph_edge *failure_edge = nullptr;
  const char *failure_reason = nullptr;
  bool entry_status_known = false;
  bool entry_has_initialized = false;

  void set_single_node (cgraph_node *);
  void set_component (const auto_vec<cgraph_node *, 16> &);
  void set_entry_result (bool, const char *, cgraph_edge * = nullptr);
  void add_failed_caller_edge (cgraph_edge *);

private:
  bool recording = true;
};

using entry_proof_node_hash = nofree_ptr_hash<cgraph_node>;
using entry_proof_tree_map_traits
	= simple_hashmap_traits<entry_proof_node_hash,
				entry_proof_tree_node *>;
using entry_proof_tree_map = hash_map<cgraph_node *, entry_proof_tree_node *,
				      entry_proof_tree_map_traits>;

struct cgraph_scc_component
{
  auto_vec<cgraph_node *, 4> nodes;
};

struct cgraph_scc_state
{
  int index = -1;
  int lowlink = -1;
  bool on_stack = false;
};

using cgraph_scc_state_map_traits
	= simple_hashmap_traits<entry_proof_node_hash, cgraph_scc_state>;
using cgraph_scc_state_map = hash_map<cgraph_node *, cgraph_scc_state,
				      cgraph_scc_state_map_traits>;
using cgraph_scc_component_map_traits
	= simple_hashmap_traits<entry_proof_node_hash,
				cgraph_scc_component *>;
using cgraph_scc_component_map = hash_map<cgraph_node *,
					  cgraph_scc_component *,
					  cgraph_scc_component_map_traits>;

class cgraph_scc_cache
{
public:
  ~cgraph_scc_cache ();

  void clear ();
  void collect_component (cgraph_node *, auto_vec<cgraph_node *, 16> &);

private:
  void ensure_built ();
  void strongconnect (cgraph_node *);

  cgraph_scc_state_map states;
  cgraph_scc_component_map components;
  auto_vec<cgraph_node *, 64> stack;
  auto_vec<cgraph_scc_component *, 64> allocated_components;
  int next_index = 0;
  bool built = false;
};

class entry_proof_cache
{
public:
  ~entry_proof_cache ();

  void set_candidate (array_candidate &);
  bool function_entry_has_initialized_p (cgraph_node *,
					 auto_vec<cgraph_node *, 16> &);
  entry_proof_tree_node *lookup (cgraph_node *);
  void clear ();
  void collect_component (cgraph_node *node,
			  auto_vec<cgraph_node *, 16> &component)
  { scc_cache.collect_component (node, component); }

private:
  entry_proof_tree_node &entry_tree_for_node (cgraph_node *);
  void record_entry_component (const auto_vec<cgraph_node *, 16> &,
			       entry_proof_tree_node &);
  bool callsite_has_initialized_p (cgraph_edge *,
				   auto_vec<cgraph_node *, 16> &,
				   entry_proof_tree_node &);
  bool prove_callsite_initialized (cgraph_edge *,
				   auto_vec<cgraph_node *, 16> &,
				   entry_proof_tree_node &);
  bool direct_callers_prove_entry_init_p
    (const auto_vec<cgraph_node *, 16> &, auto_vec<cgraph_node *, 16> &,
     entry_proof_tree_node &, bool &);
  bool finish_entry_proof (bool, bool, entry_proof_tree_node &);

  array_candidate *cand = nullptr;
  entry_proof_tree_map nodes;
  cgraph_scc_cache scc_cache;
  auto_vec<entry_proof_tree_node *, 16> allocated_nodes;
};

class dominance_failure
{
public:
  array_access *load = nullptr;
  const char *reason = nullptr;
  cgraph_edge *edge = nullptr;

  void clear ();
  void set (array_access &, const char *, cgraph_edge *);
  void set_at_load (array_access &, const char *);
};

static bool
tree_uhwi_p (tree t, unsigned &value)
{
  if (!t || TREE_CODE (t) != INTEGER_CST || !tree_fits_uhwi_p (t))
    return false;

  if (tree_to_uhwi (t) > UINT_MAX)
    return false;

  value = (unsigned) tree_to_uhwi (t);
  return true;
}

static tree
strip_ssa_copy (tree var)
{
  while (var && TREE_CODE (var) == SSA_NAME)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (var);
      if (!gimple_assign_single_p (stmt) && !gimple_assign_cast_p (stmt))
	break;

      tree rhs = gimple_assign_rhs1 (stmt);
      if (!rhs || TREE_CODE (rhs) != SSA_NAME)
	break;

      if (gimple_assign_cast_p (stmt)
	  && !tree_nop_conversion_p (TREE_TYPE (var), TREE_TYPE (rhs)))
	break;

      var = rhs;
    }

  return var;
}

static void
dump_candidate_reject (array_candidate &cand)
{
  if (dump_file && cand.reject_reason)
    fprintf (dump_file, "array-load-inline: reject %s: %s\n",
	     cand.name (), cand.reject_reason);
}

class scev_context
{
public:
  explicit scev_context (bool enabled = true)
    : active (enabled && !scev_initialized_p ())
  {
    if (active)
      scev_initialize ();
  }

  ~scev_context ()
  {
    if (active)
      scev_finalize ();
  }

private:
  scev_context (const scev_context &) = delete;
  scev_context &operator= (const scev_context &) = delete;
  bool active;
};

void
dominance_failure::clear ()
{
  load = nullptr;
  reason = nullptr;
  edge = nullptr;
}

void
dominance_failure::set (array_access &failed_load, const char *failed_reason,
			 cgraph_edge *failed_edge)
{
  if (reason)
    return;

  load = &failed_load;
  reason = failed_reason;
  edge = failed_edge;
}

void
dominance_failure::set_at_load (array_access &failed_load,
				const char *failed_reason)
{
  set (failed_load, failed_reason, nullptr);
}

bool
array_candidate::init (varpool_node *node)
{
  var = node;
  decl = node->decl;
  return fixed_array_dims_p (TREE_TYPE (decl), dims, elem_type);
}

array_access *
array_candidate::record_access (cgraph_node *node, gimple *stmt,
				bool is_store, tree rhs,
				const auto_vec<tree> &indices)
{
  array_access *access = new array_access;
  access->node = node;
  access->stmt = stmt;
  access->rhs = rhs;
  for (unsigned i = 0; i < indices.length (); ++i)
    access->indices.safe_push (indices[i]);

  if (is_store)
    stores.safe_push (access);
  else
    loads.safe_push (access);

  return access;
}

void
entry_proof_tree_node::set_single_node (cgraph_node *node)
{
  if (recording && node && component.is_empty ())
    component.safe_push (node);
}

void
entry_proof_tree_node::set_component
  (const auto_vec<cgraph_node *, 16> &nodes)
{
  if (!recording || !component.is_empty ())
    return;

  for (unsigned i = 0; i < nodes.length (); ++i)
    component.safe_push (nodes[i]);
}

void
entry_proof_tree_node::set_entry_result (bool result, const char *reason,
					 cgraph_edge *edge)
{
  if (!recording)
    return;

  entry_status_known = true;
  entry_has_initialized = result;
  if (!result)
    {
      failure_reason = reason;
      failure_edge = edge;
      add_failed_caller_edge (edge);
    }
}

void
entry_proof_tree_node::add_failed_caller_edge (cgraph_edge *edge)
{
  if (!recording || !edge)
    return;

  if (failed_caller_edges.contains (edge))
    return;
  failed_caller_edges.safe_push (edge);
}

static void
dump_entry_proof_indent (unsigned indent)
{
  fprintf (dump_file, "array-load-inline: ");
  for (unsigned i = 0; i < indent; ++i)
    fputc (' ', dump_file);
}

static void
dump_entry_proof_component (entry_proof_tree_node &tree)
{
  if (tree.component.is_empty ())
    {
      fprintf (dump_file, "(unknown)");
      return;
    }

  if (tree.component.length () == 1)
    {
      fprintf (dump_file, "%s", tree.component[0]->dump_name ());
      return;
    }

  fprintf (dump_file, "{");
  for (unsigned i = 0; i < tree.component.length (); ++i)
    {
      if (i)
	fprintf (dump_file, ", ");
      fprintf (dump_file, "%s", tree.component[i]->dump_name ());
    }
  fprintf (dump_file, "}");
}

static void
dump_entry_proof_node (entry_proof_tree_node &tree, unsigned indent)
{
  dump_entry_proof_indent (indent);
  fprintf (dump_file, "- entry node: ");
  dump_entry_proof_component (tree);
  fprintf (dump_file, "\n");

  if (tree.entry_status_known)
    {
      dump_entry_proof_indent (indent + 2);
      fprintf (dump_file, "entry_has_initialized: %s\n",
	       tree.entry_has_initialized ? "yes" : "no");
    }

  if (tree.failure_reason)
    {
      dump_entry_proof_indent (indent + 2);
      fprintf (dump_file, "reason: %s\n", tree.failure_reason);
    }

  if (tree.failure_edge)
    {
      dump_entry_proof_indent (indent + 2);
      fprintf (dump_file, "failure edge: %s -> %s\n",
	       tree.failure_edge->caller
	       ? tree.failure_edge->caller->dump_name () : "(unknown)",
	       tree.failure_edge->callee
	       ? tree.failure_edge->callee->dump_name () : "(unknown)");
    }
}

static void
dump_entry_proof_failure_path (entry_proof_cache &cache, cgraph_node *node,
			       unsigned indent,
			       auto_vec<entry_proof_tree_node *, 8> &seen)
{
  if (!node)
    return;

  entry_proof_tree_node *tree = cache.lookup (node);
  if (!tree)
    return;

  dump_entry_proof_node (*tree, indent);
  if (seen.contains (tree))
    {
      dump_entry_proof_indent (indent + 2);
      fprintf (dump_file, "cycle in failure path\n");
      return;
    }
  seen.safe_push (tree);

  for (unsigned i = 0; i < tree->failed_caller_edges.length (); ++i)
    {
      cgraph_edge *edge = tree->failed_caller_edges[i];
      if (!edge)
	continue;

      dump_entry_proof_indent (indent + 2);
      fprintf (dump_file, "failed caller edge: %s -> %s\n",
	       edge->caller ? edge->caller->dump_name () : "(unknown)",
	       edge->callee ? edge->callee->dump_name () : "(unknown)");

      if (edge->call_stmt)
	{
	  dump_entry_proof_indent (indent + 2);
	  fprintf (dump_file, "call stmt: ");
	  print_gimple_stmt (dump_file, edge->call_stmt, 0, TDF_SLIM);
	}

      dump_entry_proof_failure_path (cache, edge->caller, indent + 2, seen);
    }

  seen.pop ();
}

static void
dump_dominance_failure (array_candidate &cand, dominance_failure &failure,
			entry_proof_cache &cache)
{
  if (!dump_file || !(dump_flags & TDF_DETAILS) || !failure.load)
    return;

  fprintf (dump_file,
	   "array-load-inline: details: cannot prove initialization of %s "
	   "dominates load\n", cand.name ());

  if (failure.load->node)
    fprintf (dump_file, "array-load-inline:   load function: %s\n",
	     failure.load->node->dump_name ());

  fprintf (dump_file, "array-load-inline:   load stmt: ");
  print_gimple_stmt (dump_file, failure.load->stmt, 0, TDF_SLIM);

  if (failure.reason)
    fprintf (dump_file, "array-load-inline:   failed proof: %s\n",
	     failure.reason);

  fprintf (dump_file, "array-load-inline:   entry proof failure path:\n");
  auto_vec<entry_proof_tree_node *, 8> seen;
  dump_entry_proof_failure_path (cache, failure.load->node, 4, seen);

  if (failure.edge && failure.edge->call_stmt)
    {
      fprintf (dump_file, "array-load-inline:   call stmt: ");
      print_gimple_stmt (dump_file, failure.edge->call_stmt, 0, TDF_SLIM);
    }
}

static bool
stmt_dominated_by_p (gimple *stmt, gimple *dom)
{
  basic_block bb = gimple_bb (stmt);
  basic_block dom_bb = gimple_bb (dom);

  if (!bb || !dom_bb)
    return false;

  if (bb != dom_bb)
    return dominated_by_p (CDI_DOMINATORS, bb, dom_bb);

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *cur = gsi_stmt (gsi);
      if (cur == stmt)
	return false;
      if (cur == dom)
	return true;
    }

  return false;
}

static bool
stmt_executes_before_bb_end_p (gimple *stmt, basic_block bb)
{
  basic_block stmt_bb = gimple_bb (stmt);

  if (!stmt_bb || !bb)
    return false;

  if (stmt_bb != bb)
    return dominated_by_p (CDI_DOMINATORS, bb, stmt_bb);

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    if (gsi_stmt (gsi) == stmt)
      return true;

  return false;
}

static bool
loop_has_positive_constant_iterations_p (loop_p loop)
{
  tree niters = number_of_latch_executions (loop);
  unsigned niter_value;
  return tree_uhwi_p (niters, niter_value) && niter_value > 0;
}

static bool
loop_contains_p (loop_p outer, loop_p inner)
{
  for (loop_p loop = inner; loop; loop = loop_outer (loop))
    if (loop == outer)
      return true;

  return false;
}

static loop_p
nested_child_loop (loop_p outer, const auto_vec<loop_p, 4> &loops)
{
  loop_p best = nullptr;
  for (unsigned i = 0; i < loops.length (); ++i)
    {
      loop_p candidate = loops[i];
      if (candidate == outer || !loop_contains_p (outer, candidate))
	continue;

      if (!best || loop_depth (candidate) < loop_depth (best))
	best = candidate;
    }

  return best;
}

static bool
loop_initialization_precedes_stmt_p (array_access &store, gimple *stmt)
{
  auto_vec<loop_p, 4> loops;
  for (unsigned i = 0; i < store.indices.length (); ++i)
    {
      /* These loops were recorded only after validating every index cast.
	 The pass retains loop structures until all candidates are processed.  */
      loop_p loop = store.index_loops[i];
      if (!loop)
	continue;

      if (!loops.contains (loop))
	loops.safe_push (loop);
    }

  if (loops.is_empty ())
    return false;

  loop_p outermost = loops[0];
  loop_p innermost = loops[0];
  for (unsigned i = 0; i < loops.length (); ++i)
    {
      loop_p loop = loops[i];
      if (!loop_has_positive_constant_iterations_p (loop))
	return false;

      if (loop_depth (loop) < loop_depth (outermost))
	outermost = loop;
      if (loop_depth (loop) > loop_depth (innermost))
	innermost = loop;
    }

  basic_block stmt_bb = gimple_bb (stmt);
  if (flow_bb_inside_loop_p (outermost, stmt_bb)
      || !dominated_by_p (CDI_DOMINATORS, stmt_bb, outermost->header))
    return false;

  for (unsigned i = 0; i < loops.length (); ++i)
    {
      loop_p loop = loops[i];
      edge latch = loop_latch_edge (loop);
      if (!latch)
	return false;

      loop_p child = nested_child_loop (loop, loops);
      if (child)
	{
	  if (!dominated_by_p (CDI_DOMINATORS, latch->src, child->header))
	    return false;
	}
      else if (!stmt_executes_before_bb_end_p (store.stmt, latch->src))
	return false;
    }

  return innermost != outermost
	 || stmt_executes_before_bb_end_p (store.stmt,
					   loop_latch_edge (innermost)->src);
}

/* Statement dominance only proves that one dynamic store has executed.
   For a varying region it cannot establish that all elements were written.  */
static bool
single_element_store_p (const array_access &store)
{
  for (unsigned i = 0; i < store.lows.length (); ++i)
    if (store.lows[i] != store.highs[i])
      return false;
  return true;
}

static bool
store_initialization_precedes_stmt_p (array_access &store, gimple *stmt)
{
  if (single_element_store_p (store))
    return stmt_dominated_by_p (stmt, store.stmt);
  return loop_initialization_precedes_stmt_p (store, stmt);
}

static bool
analyzable_node_p (cgraph_node *node)
{
  return node && node->real_symbol_p () && node->definition
	 && node->has_gimple_body_p () && !node->inlined_to;
}

/* Only these nodes may derive an entry fact from their recorded callers.
   Public symbols, aliases and indirect entries can have unrecorded callers.  */
static bool
closed_entry_p (cgraph_node *node)
{
  return analyzable_node_p (node)
	 && !node->externally_visible && !DECL_EXTERNAL (node->decl)
	 && !node->used_from_other_partition && !node->address_taken
	 && !node->force_output && !node->has_aliases_p ();
}

static void
dump_unanalyzable_node_details (cgraph_node *node)
{
  if (!dump_file || !(dump_flags & TDF_DETAILS) || !node)
    return;

  fprintf (dump_file, "array-load-inline:   unanalyzable node: %s\n",
	   node->dump_name ());
  fprintf (dump_file,
	   "array-load-inline:     real_symbol=%u definition=%u "
	   "has_gimple_body=%u inlined_to=%u address_taken=%u\n",
	   node->real_symbol_p (), node->definition,
	   node->has_gimple_body_p (), node->inlined_to != nullptr,
	   node->address_taken);

  if (!node->address_taken)
    return;

  bool saw_addr_ref = false;
  ipa_ref *ref = nullptr;
  for (unsigned i = 0; node->iterate_referring (i, ref); ++i)
    {
      if (ref->use != IPA_REF_ADDR)
	continue;

      saw_addr_ref = true;
      fprintf (dump_file, "array-load-inline:     address reference from %s",
	       ref->referring ? ref->referring->dump_name () : "(unknown)");
      if (ref->referring && VAR_P (ref->referring->decl)
	  && DECL_VIRTUAL_P (ref->referring->decl))
	fprintf (dump_file, " (virtual table)");
      if (ref->stmt)
	{
	  fprintf (dump_file, ": ");
	  print_gimple_stmt (dump_file, ref->stmt, 0, TDF_SLIM);
	}
      else
	fprintf (dump_file, "\n");
    }

  if (!saw_addr_ref)
    fprintf (dump_file,
	     "array-load-inline:     no direct IPA_REF_ADDR referring edge "
	     "recorded\n");
}

cgraph_scc_cache::~cgraph_scc_cache ()
{
  clear ();
}

void
cgraph_scc_cache::clear ()
{
  for (unsigned i = 0; i < allocated_components.length (); ++i)
    delete allocated_components[i];
  allocated_components.truncate (0);
  states.empty ();
  components.empty ();
  stack.truncate (0);
  next_index = 0;
  built = false;
}

void
cgraph_scc_cache::strongconnect (cgraph_node *node)
{
  cgraph_scc_state &state = states.get_or_insert (node);
  state.index = next_index;
  state.lowlink = next_index;
  state.on_stack = true;
  ++next_index;
  stack.safe_push (node);

  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    {
      cgraph_node *callee = e->callee;
      if (!analyzable_node_p (callee))
	continue;

      cgraph_scc_state *callee_state = states.get (callee);
      if (!callee_state)
	{
	  strongconnect (callee);
	  callee_state = states.get (callee);
	  cgraph_scc_state *node_state = states.get (node);
	  node_state->lowlink = MIN (node_state->lowlink,
				     callee_state->lowlink);
	}
      else if (callee_state->on_stack)
	{
	  cgraph_scc_state *node_state = states.get (node);
	  node_state->lowlink = MIN (node_state->lowlink,
				     callee_state->index);
	}
    }

  cgraph_scc_state *node_state = states.get (node);
  if (node_state->lowlink != node_state->index)
    return;

  cgraph_scc_component *component = new cgraph_scc_component;
  allocated_components.safe_push (component);
  while (true)
    {
      cgraph_node *member = stack.pop ();
      cgraph_scc_state *member_state = states.get (member);
      member_state->on_stack = false;
      component->nodes.safe_push (member);
      components.put (member, component);
      if (member == node)
	break;
    }
}

void
cgraph_scc_cache::ensure_built ()
{
  if (built)
    return;

  built = true;
  cgraph_node *node = nullptr;
  FOR_EACH_FUNCTION (node)
    if (analyzable_node_p (node) && !states.get (node))
      strongconnect (node);
}

void
cgraph_scc_cache::collect_component (cgraph_node *root,
				     auto_vec<cgraph_node *, 16> &component)
{
  component.truncate (0);
  ensure_built ();

  cgraph_scc_component **slot = components.get (root);
  if (!slot)
    {
      component.safe_push (root);
      return;
    }

  cgraph_scc_component *root_component = *slot;
  for (unsigned i = 0; i < root_component->nodes.length (); ++i)
    component.safe_push (root_component->nodes[i]);
}

static bool
candidate_has_explicit_load_ref_p (array_candidate &cand)
{
  ipa_ref *ref = nullptr;
  for (unsigned i = 0; cand.var->iterate_referring (i, ref); ++i)
    if (ref->use == IPA_REF_LOAD)
      return true;

  return false;
}

static bool node_calls_store_node_before_return_p (array_candidate &,
						   cgraph_node *,
						   cgraph_node *,
						   auto_vec<cgraph_node *, 16> &);

static bool
stmt_dominates_function_exit_p (gimple *stmt)
{
  if (!gimple_bb (stmt) || !cfun)
    return false;

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    if (!stmt_executes_before_bb_end_p (stmt, e->src))
      return false;

  return true;
}

static bool
loop_initialization_precedes_function_exit_p (array_access &store)
{
  auto_vec<loop_p, 4> loops;
  for (unsigned i = 0; i < store.indices.length (); ++i)
    {
      /* These loops were recorded only after validating every index cast.
	 The pass retains loop structures until all candidates are processed.  */
      loop_p loop = store.index_loops[i];
      if (!loop)
	continue;

      if (!loops.contains (loop))
	loops.safe_push (loop);
    }

  if (loops.is_empty ())
    return false;

  loop_p outermost = loops[0];
  for (unsigned i = 0; i < loops.length (); ++i)
    {
      loop_p loop = loops[i];
      if (!loop_has_positive_constant_iterations_p (loop))
	return false;

      if (loop_depth (loop) < loop_depth (outermost))
	outermost = loop;
    }

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      if (flow_bb_inside_loop_p (outermost, e->src)
	  || !dominated_by_p (CDI_DOMINATORS, e->src, outermost->header))
	return false;
    }

  for (unsigned i = 0; i < loops.length (); ++i)
    {
      loop_p loop = loops[i];
      edge latch = loop_latch_edge (loop);
      if (!latch)
	return false;

      loop_p child = nested_child_loop (loop, loops);
      if (child)
	{
	  if (!dominated_by_p (CDI_DOMINATORS, latch->src, child->header))
	    return false;
	}
      else if (!stmt_executes_before_bb_end_p (store.stmt, latch->src))
	return false;
    }

  return true;
}

static bool
store_node_initializes_before_return_p (array_candidate &cand,
					cgraph_node *store_node)
{
  if (!analyzable_node_p (store_node))
    return false;

  scoped_dump_file no_dump;
  cfun_saver save (cand.analyses, store_node,
		  LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  scev_context scev;

  bool saw_store = false;
  for (unsigned i = 0; i < cand.stores.length (); ++i)
    {
      array_access *store = cand.stores[i];
      if (store->node != store_node)
	continue;

      saw_store = true;
      if (single_element_store_p (*store)
	  ? !stmt_dominates_function_exit_p (store->stmt)
	  : !loop_initialization_precedes_function_exit_p (*store))
	return false;
    }

  return saw_store;
}

/* Coverage currently admits one store region.  Memoize both successful and
   failed callee summaries, so a diamond-shaped call graph is visited once.
   A recursive dependency is conservatively false; it is not an entry fact.  */
static bool
callee_guarantees_store_node_p (array_candidate &cand, cgraph_node *callee,
				cgraph_node *store_node,
				auto_vec<cgraph_node *, 16> &stack)
{
  gcc_assert (cand.stores.length () == 1);
  if (bool *cached = cand.initializes_on_return.get (callee))
    return *cached;
  if (stack.contains (callee))
    return false;

  bool result = callee == store_node
		? store_node_initializes_before_return_p (cand, store_node)
		: node_calls_store_node_before_return_p (cand, callee,
							 store_node, stack);
  cand.initializes_on_return.put (callee, result);
  return result;
}

static bool
all_store_nodes_called_before_stmt_p (array_candidate &cand, cgraph_node *caller,
				      gimple *stmt)
{
  if (bool *cached = cand.initialized_before.get (stmt))
    return *cached;

  gcc_assert (cand.stores.length () == 1);
  scoped_dump_file no_dump;
  array_access &store = *cand.stores[0];
  bool local = store.node == caller;
  cfun_saver save (cand.analyses, caller,
		  local ? LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS : 0);
  scev_context scev (local);
  bool initialized = false;
  if (store.node == caller)
    initialized = store_initialization_precedes_stmt_p (store, stmt);
  else
    for (cgraph_edge *e = caller->callees; e; e = e->next_callee)
      {
	auto_vec<cgraph_node *, 16> stack;
	if (e->callee && e->call_stmt && e->binds_to_current_def_p ()
	    && stmt_dominated_by_p (stmt, e->call_stmt)
	    && callee_guarantees_store_node_p (cand, e->callee,
					       store.node, stack))
	  {
	    initialized = true;
	    break;
	  }
      }
  cand.initialized_before.put (stmt, initialized);
  return initialized;
}

static bool
node_calls_store_node_before_return_p (array_candidate &cand,
				       cgraph_node *node,
				       cgraph_node *store_node,
				       auto_vec<cgraph_node *, 16> &stack)
{
  if (!analyzable_node_p (node) || stack.contains (node))
    return false;

  stack.safe_push (node);

  scoped_dump_file no_dump;
  cfun_saver save (cand.analyses, node);

  bool found = false;
  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    {
      if (!e->callee || !e->call_stmt || !e->binds_to_current_def_p ()
	  || !stmt_dominates_function_exit_p (e->call_stmt))
	continue;

      if (callee_guarantees_store_node_p (cand, e->callee, store_node, stack))
	{
	  found = true;
	  break;
	}
    }

  stack.pop ();
  return found;
}

entry_proof_cache::~entry_proof_cache ()
{
  clear ();
}

void
entry_proof_cache::set_candidate (array_candidate &candidate)
{
  clear ();
  cand = &candidate;
}

entry_proof_tree_node *
entry_proof_cache::lookup (cgraph_node *node)
{
  entry_proof_tree_node **slot = nodes.get (node);
  return slot ? *slot : nullptr;
}

void
entry_proof_cache::clear ()
{
  for (unsigned i = 0; i < allocated_nodes.length (); ++i)
    delete allocated_nodes[i];
  allocated_nodes.truncate (0);
  nodes.empty ();
  cand = nullptr;
}

entry_proof_tree_node &
entry_proof_cache::entry_tree_for_node (cgraph_node *node)
{
  bool existed = false;
  entry_proof_tree_node *&tree = nodes.get_or_insert (node, &existed);
  if (!existed)
    {
      tree = new entry_proof_tree_node;
      allocated_nodes.safe_push (tree);
    }
  return *tree;
}

void
entry_proof_cache::record_entry_component
  (const auto_vec<cgraph_node *, 16> &component, entry_proof_tree_node &tree)
{
  for (unsigned i = 0; i < component.length (); ++i)
    {
      bool existed = false;
      entry_proof_tree_node *&slot
	= nodes.get_or_insert (component[i], &existed);
      if (!existed)
	slot = &tree;
      else
	gcc_assert (slot == &tree);
    }
}

bool
entry_proof_cache::callsite_has_initialized_p
  (cgraph_edge *edge, auto_vec<cgraph_node *, 16> &stack,
   entry_proof_tree_node &callee_tree)
{
  if (!edge->caller || !edge->call_stmt)
    {
      callee_tree.set_entry_result (false, "caller edge is not fully visible",
				    edge);
      return false;
    }

  if (function_entry_has_initialized_p (edge->caller, stack)
      || all_store_nodes_called_before_stmt_p (*cand, edge->caller,
						      edge->call_stmt))
    return true;

  callee_tree.add_failed_caller_edge (edge);
  return false;
}

static unsigned
push_entry_component (const auto_vec<cgraph_node *, 16> &component,
		      auto_vec<cgraph_node *, 16> &stack)
{
  for (unsigned i = 0; i < component.length (); ++i)
    stack.safe_push (component[i]);
  return stack.length () - component.length ();
}

bool
entry_proof_cache::prove_callsite_initialized
  (cgraph_edge *edge, auto_vec<cgraph_node *, 16> &stack,
   entry_proof_tree_node &tree)
{
  if (callsite_has_initialized_p (edge, stack, tree))
    return true;

  tree.set_entry_result (false,
			 "external caller does not prove initialization",
			 edge);
  return false;
}

bool
entry_proof_cache::direct_callers_prove_entry_init_p
  (const auto_vec<cgraph_node *, 16> &component,
   auto_vec<cgraph_node *, 16> &stack, entry_proof_tree_node &tree,
   bool &saw_caller)
{
  for (unsigned i = 0; i < component.length (); ++i)
    for (cgraph_edge *e = component[i]->callers; e; e = e->next_caller)
      {
	if (!e->caller || !e->call_stmt || e->indirect_inlining_edge)
	  {
	    tree.set_entry_result (false, "caller edge is not fully visible",
				   e);
	    return false;
	  }

	if (component.contains (e->caller))
	  continue;

	if (stack.contains (e->caller))
	  {
	    tree.add_failed_caller_edge (e);
	    return false;
	  }

	saw_caller = true;
	if (!prove_callsite_initialized (e, stack, tree))
	  return false;
      }

  return true;
}

bool
entry_proof_cache::finish_entry_proof
  (bool saw_caller, bool ok, entry_proof_tree_node &tree)
{
  if (!saw_caller && ok)
    tree.set_entry_result (false, "function entry has no visible callers");
  else if (ok)
    tree.set_entry_result (true, nullptr);

  return saw_caller && ok;
}

bool
entry_proof_cache::function_entry_has_initialized_p
  (cgraph_node *node, auto_vec<cgraph_node *, 16> &stack)
{
  entry_proof_tree_node *cached_tree = lookup (node);
  if (cached_tree && cached_tree->entry_status_known)
    return cached_tree->entry_has_initialized;

  if (!closed_entry_p (node))
    {
      entry_proof_tree_node &tree = entry_tree_for_node (node);
      tree.set_single_node (node);
      tree.set_entry_result (false, "function is not fully visible");
      dump_unanalyzable_node_details (node);
      return false;
    }

  auto_vec<cgraph_node *, 16> component;
  scc_cache.collect_component (node, component);
  for (unsigned i = 0; i < component.length (); ++i)
    if (!closed_entry_p (component[i]) || stack.contains (component[i]))
      return false;

  entry_proof_tree_node &tree = entry_tree_for_node (node);
  if (tree.entry_status_known)
    return tree.entry_has_initialized;

  tree.set_component (component);
  record_entry_component (component, tree);

  unsigned stack_len = push_entry_component (component, stack);

  bool saw_caller = false;
  bool ok = direct_callers_prove_entry_init_p (component, stack, tree,
					       saw_caller);

  stack.truncate (stack_len);
  return finish_entry_proof (saw_caller, ok, tree);
}

static bool
zero_initialized_decl_p (tree decl)
{
  if (!decl || !VAR_P (decl) || DECL_EXTERNAL (decl))
    return false;

  tree init = DECL_INITIAL (decl);
  return !init || init == error_mark_node || initializer_zerop (init);
}

static bool
visible_node_with_body_p (cgraph_node *node)
{
  return node && node->real_symbol_p () && node->definition
	 && node->has_gimple_body_p () && !node->inlined_to;
}

struct guard_decl_result
{
  tree decl = nullptr;
  bool safe = false;
};

struct guard_store
{
  cgraph_node *node = nullptr;
  gimple *stmt = nullptr;
};

struct guard_path_states
{
  hash_set<basic_block> uninitialized;
  hash_set<basic_block> initialized;

  bool add (basic_block bb, bool init)
  {
    return init ? initialized.add (bb) : uninitialized.add (bb);
  }
};

class guarded_load_prover
{
public:
  guarded_load_prover (array_candidate &candidate, entry_proof_cache &cache)
    : cand (candidate), entry_cache (cache)
  {}

  bool load_safe (array_access &);

private:
  bool guard_decl_candidate_p (tree, auto_vec<unsigned> &);
  bool scan_guard_decl_store_nodes (tree, const auto_vec<unsigned> &,
				    auto_vec<guard_store, 8> &);
  bool guard_decl_zero_until_target_initialized_p (tree);
  bool guard_stores_guarantee_target_init_p
    (const auto_vec<guard_store, 8> &);
  bool guard_store_guarantees_target_init_p (guard_store &);
  bool target_load_stmt_p (gimple *);
  bool callee_guarantees_all_store_nodes_p
    (cgraph_node *, auto_vec<cgraph_node *, 16> &);
  bool function_has_no_target_load_before_init_p
    (cgraph_node *, bool, auto_vec<cgraph_node *, 16> &);
  bool paths_from_stmt_have_no_target_load_before_init_p
    (cgraph_node *, gimple *);
  bool path_stmt_has_no_target_load_before_init_p
    (cgraph_node *, gimple *, bool &, auto_vec<cgraph_node *, 16> &);
  bool paths_from_bb_have_no_target_load_before_init_p
    (cgraph_node *, basic_block, gimple *, bool,
     guard_path_states &, auto_vec<cgraph_node *, 16> &);
  bool expr_zero_if_guard_zero_p (tree, tree &, auto_vec<tree, 8> &);
  bool ssa_name_zero_if_guard_zero_p (tree, tree &, auto_vec<tree, 8> &);
  bool edge_requires_guard_nonzero_p (edge, tree &);
  bool bb_guarded_by_false_condition_without_init_p (cgraph_node *,
						     basic_block);
  bool function_entry_safe_without_init_p (cgraph_node *,
					   auto_vec<cgraph_node *, 16> &);
  bool callsite_safe_without_init_p (cgraph_edge *,
				     auto_vec<cgraph_node *, 16> &);

  array_candidate &cand;
  entry_proof_cache &entry_cache;
  auto_vec<guard_decl_result, 4> guard_cache;
  hash_map<gimple *, bool> load_safe_cache;
  hash_map<basic_block, bool> bb_guard_cache;
  hash_map<cgraph_edge *, bool> callsite_safe_cache;
  hash_map<cgraph_node *, bool> function_entry_safe_cache;
};

bool
guarded_load_prover::guard_decl_candidate_p (tree guard_decl,
					     auto_vec<unsigned> &dims)
{
  if (!guard_decl || guard_decl == cand.decl
      || !zero_initialized_decl_p (guard_decl))
    return false;

  varpool_node *guard_var = varpool_node::get (guard_decl);
  if (guard_var && !guard_var->all_refs_explicit_p ())
    return false;

  tree elem_type = nullptr;
  return fixed_array_dims_p (TREE_TYPE (guard_decl), dims, elem_type);
}

static bool
guard_store_seen_p (const auto_vec<guard_store, 8> &stores, gimple *stmt)
{
  for (unsigned i = 0; i < stores.length (); ++i)
    if (stores[i].stmt == stmt)
      return true;

  return false;
}

static bool
known_zero_value_p (tree expr, auto_vec<tree, 8> &visiting)
{
  if (!expr)
    return false;

  if (integer_zerop (expr))
    return true;

  if (TREE_CODE (expr) != SSA_NAME)
    return false;

  if (visiting.contains (expr))
    return false;

  visiting.safe_push (expr);
  gimple *def = SSA_NAME_DEF_STMT (expr);
  bool ok = (gimple_assign_single_p (def) || gimple_assign_cast_p (def))
	    && known_zero_value_p (gimple_assign_rhs1 (def), visiting);

  visiting.pop ();
  return ok;
}

static bool
known_zero_value_p (tree expr)
{
  auto_vec<tree, 8> visiting;
  return known_zero_value_p (expr, visiting);
}

static bool
scan_guard_decl_stmt (cgraph_node *node, gimple *stmt, tree guard_decl,
		      const auto_vec<unsigned> &dims,
		      auto_vec<guard_store, 8> &guard_stores)
{
  if (!is_gimple_assign (stmt))
    {
      for (unsigned i = 0; i < gimple_num_ops (stmt); ++i)
	if (expr_contains_decl_p (gimple_op (stmt, i), guard_decl))
	  return false;
      return true;
    }

  tree lhs = gimple_get_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);
  tree ref_decl = nullptr;
  auto_vec<tree> lhs_indices;
  auto_vec<tree> rhs_indices;
  bool lhs_ref = lhs && extract_array_ref (lhs, ref_decl, lhs_indices)
		 && ref_decl == guard_decl;
  bool rhs_ref = rhs && extract_array_ref (rhs, ref_decl, rhs_indices)
		 && ref_decl == guard_decl;

  if ((lhs && expr_contains_decl_p (lhs, guard_decl) && !lhs_ref)
      || (rhs && expr_contains_decl_p (rhs, guard_decl) && !rhs_ref)
      || (gimple_num_ops (stmt) > 2
	  && expr_contains_decl_p (gimple_op (stmt, 2), guard_decl)))
    return false;

  if (!lhs_ref)
    return true;

  if ((lhs_indices.length () && lhs_indices.length () != dims.length ())
      || expr_contains_decl_p (rhs, guard_decl))
    return false;

  if (!known_zero_value_p (rhs) && !guard_store_seen_p (guard_stores, stmt))
    {
      guard_store store;
      store.node = node;
      store.stmt = stmt;
      guard_stores.safe_push (store);
    }

  return true;
}

bool
guarded_load_prover::scan_guard_decl_store_nodes
  (tree guard_decl, const auto_vec<unsigned> &dims,
   auto_vec<guard_store, 8> &guard_stores)
{
  guard_stores.truncate (0);
  varpool_node *var = varpool_node::get (guard_decl);
  if (!var || !var->all_refs_explicit_p ())
    return false;

  hash_set<gimple *> seen;
  ipa_ref *ref;
  for (unsigned i = 0; var->iterate_referring (i, ref); ++i)
    {
      if (ref->use == IPA_REF_ADDR || ref->use == IPA_REF_ALIAS || !ref->stmt)
	return false;
      cgraph_node *node = dyn_cast<cgraph_node *> (ref->referring);
      if (!visible_node_with_body_p (node))
	return false;
      if (seen.add (ref->stmt))
	continue;
      if (!scan_guard_decl_stmt (node, ref->stmt, guard_decl, dims,
				 guard_stores))
	return false;
    }
  return true;
}

bool
guarded_load_prover::
guard_decl_zero_until_target_initialized_p (tree guard_decl)
{
  for (unsigned i = 0; i < guard_cache.length (); ++i)
    if (guard_cache[i].decl == guard_decl)
      return guard_cache[i].safe;

  guard_decl_result result;
  result.decl = guard_decl;

  auto_vec<unsigned> dims;
  auto_vec<guard_store, 8> guard_stores;
  result.safe = false;
  if (guard_decl_candidate_p (guard_decl, dims)
      && scan_guard_decl_store_nodes (guard_decl, dims, guard_stores))
    result.safe = guard_stores_guarantee_target_init_p (guard_stores);
  guard_cache.safe_push (result);

  return result.safe;
}

bool
guarded_load_prover::target_load_stmt_p (gimple *stmt)
{
  if (!gimple_assign_single_p (stmt))
    return false;

  tree ref_decl = nullptr;
  auto_vec<tree> indices;
  return extract_array_ref (gimple_assign_rhs1 (stmt), ref_decl, indices)
	 && ref_decl == cand.decl
	 && indices.length () == cand.dims.length ();
}

bool
guarded_load_prover::callee_guarantees_all_store_nodes_p
  (cgraph_node *callee, auto_vec<cgraph_node *, 16> &stack)
{
  return callee_guarantees_store_node_p (cand, callee, cand.stores[0]->node,
				       stack);
}

bool
guarded_load_prover::path_stmt_has_no_target_load_before_init_p
  (cgraph_node *node, gimple *stmt, bool &initialized,
   auto_vec<cgraph_node *, 16> &call_stack)
{
  if (initialized)
    return true;

  if (all_store_nodes_called_before_stmt_p (cand, node, stmt))
    {
      initialized = true;
      return true;
    }

  if (target_load_stmt_p (stmt))
    return false;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return true;

  cgraph_edge *edge = node->get_edge (stmt);
  if (!edge || edge->indirect_unknown_callee || !edge->callee
      || !edge->binds_to_current_def_p ())
    return false;

  if (!function_has_no_target_load_before_init_p
       (edge->callee, initialized, call_stack))
    return false;

  auto_vec<cgraph_node *, 16> guarantee_stack;
  if (callee_guarantees_all_store_nodes_p (edge->callee, guarantee_stack))
    initialized = true;

  return true;
}

bool
guarded_load_prover::paths_from_bb_have_no_target_load_before_init_p
  (cgraph_node *node, basic_block bb, gimple *start_after, bool initialized,
   guard_path_states &seen,
   auto_vec<cgraph_node *, 16> &call_stack)
{
  if (!bb || bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return initialized;

  if (!start_after && seen.add (bb, initialized))
    return true;

  bool past_start = start_after == nullptr;
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (!past_start)
	{
	  if (stmt == start_after)
	    past_start = true;
	  continue;
	}

      if (!path_stmt_has_no_target_load_before_init_p
	   (node, stmt, initialized, call_stack))
	return false;
    }

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (!e->dest || e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	{
	  if (!initialized)
	    return false;
	  continue;
	}

      if (!paths_from_bb_have_no_target_load_before_init_p
	  (node, e->dest, nullptr, initialized, seen, call_stack))
	return false;
    }

  return true;
}

bool
guarded_load_prover::function_has_no_target_load_before_init_p
  (cgraph_node *node, bool initialized, auto_vec<cgraph_node *, 16> &call_stack)
{
  if (initialized)
    return true;

  if (!analyzable_node_p (node) || call_stack.contains (node))
    return false;

  call_stack.safe_push (node);

  scoped_dump_file no_dump;
  cfun_saver save (cand.analyses, node);

  guard_path_states seen;
  bool ok = true;
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs)
    if (!paths_from_bb_have_no_target_load_before_init_p
	(node, e->dest, nullptr, initialized, seen, call_stack))
      {
	ok = false;
	break;
      }

  call_stack.pop ();
  return ok;
}

bool
guarded_load_prover::paths_from_stmt_have_no_target_load_before_init_p
  (cgraph_node *node, gimple *stmt)
{
  if (!analyzable_node_p (node))
    return false;

  scoped_dump_file no_dump;
  cfun_saver save (cand.analyses, node);

  guard_path_states seen;
  auto_vec<cgraph_node *, 16> call_stack;
  call_stack.safe_push (node);
  return paths_from_bb_have_no_target_load_before_init_p
	 (node, gimple_bb (stmt), stmt, false, seen, call_stack);
}

bool
guarded_load_prover::guard_store_guarantees_target_init_p (guard_store &store)
{
  if (!store.node || !store.stmt)
    return false;

  return all_store_nodes_called_before_stmt_p (cand, store.node, store.stmt)
	 || paths_from_stmt_have_no_target_load_before_init_p (store.node,
							       store.stmt);
}

bool
guarded_load_prover::guard_stores_guarantee_target_init_p
  (const auto_vec<guard_store, 8> &guard_stores)
{
  for (unsigned i = 0; i < guard_stores.length (); ++i)
    {
      guard_store store = guard_stores[i];
      if (!guard_store_guarantees_target_init_p (store))
	return false;
    }

  return true;
}

bool
guarded_load_prover::ssa_name_zero_if_guard_zero_p
  (tree name, tree &guard_decl, auto_vec<tree, 8> &visiting)
{
  if (visiting.contains (name))
    return false;

  visiting.safe_push (name);

  gimple *def = SSA_NAME_DEF_STMT (name);
  bool ok = false;
  if (is_gimple_assign (def)
      && (gimple_assign_single_p (def) || gimple_assign_cast_p (def)
	  || gimple_assign_rhs_class (def) == GIMPLE_SINGLE_RHS
	  || gimple_assign_rhs_code (def) == ARRAY_REF))
    ok = expr_zero_if_guard_zero_p (gimple_assign_rhs1 (def), guard_decl,
				    visiting);
  else if (is_gimple_assign (def))
    {
      tree_code code = gimple_assign_rhs_code (def);
      if (code == BIT_AND_EXPR || code == MULT_EXPR)
	ok = (expr_zero_if_guard_zero_p (gimple_assign_rhs1 (def), guard_decl,
					 visiting)
	      || expr_zero_if_guard_zero_p (gimple_assign_rhs2 (def),
					    guard_decl, visiting));
      else if (code == LSHIFT_EXPR || code == RSHIFT_EXPR)
	ok = expr_zero_if_guard_zero_p (gimple_assign_rhs1 (def), guard_decl,
					visiting);
    }

  visiting.pop ();
  return ok;
}

bool
guarded_load_prover::expr_zero_if_guard_zero_p
  (tree expr, tree &guard_decl, auto_vec<tree, 8> &visiting)
{
  if (!expr)
    return false;

  if (TREE_CODE (expr) == SSA_NAME)
    return ssa_name_zero_if_guard_zero_p (expr, guard_decl, visiting);

  tree ref_decl = nullptr;
  auto_vec<tree> indices;
  if (extract_array_ref (expr, ref_decl, indices)
      && guard_decl_zero_until_target_initialized_p (ref_decl))
    {
      if (guard_decl && guard_decl != ref_decl)
	return false;

      guard_decl = ref_decl;
      return true;
    }

  return false;
}

bool
guarded_load_prover::edge_requires_guard_nonzero_p (edge e, tree &guard_decl)
{
  if (!e || !(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
    return false;

  gimple *stmt = last_nondebug_stmt (e->src);
  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return false;

  tree_code code = gimple_cond_code (stmt);
  tree lhs = gimple_cond_lhs (stmt);
  tree rhs = gimple_cond_rhs (stmt);
  tree expr = nullptr;

  if ((code == NE_EXPR && (e->flags & EDGE_TRUE_VALUE))
      || (code == EQ_EXPR && (e->flags & EDGE_FALSE_VALUE)))
    {
      if (integer_zerop (rhs))
	expr = lhs;
      else if (integer_zerop (lhs))
	expr = rhs;
    }

  if (!expr)
    return false;

  auto_vec<tree, 8> visiting;
  return expr_zero_if_guard_zero_p (expr, guard_decl, visiting) && guard_decl;
}

bool
guarded_load_prover::bb_guarded_by_false_condition_without_init_p
  (cgraph_node *node, basic_block target_bb)
{
  if (!target_bb || !analyzable_node_p (node))
    return false;

  if (bool *cached = bb_guard_cache.get (target_bb))
    return *cached;

  scoped_dump_file no_dump;
  cfun_saver save (cand.analyses, node);

  auto_vec<edge, 16> guard_edges;
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	    && single_pred_p (e->dest)
	    && dominated_by_p (CDI_DOMINATORS, target_bb, e->dest))
	  guard_edges.safe_push (e);
    }

  /* Inspect guards after collecting the controlling edges.  The recursive
     guard proofs share this pass's function analyses.  */
  for (unsigned i = 0; i < guard_edges.length (); ++i)
    {
      edge e = guard_edges[i];
      tree guard_decl = nullptr;
      if (edge_requires_guard_nonzero_p (e, guard_decl))
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "array-load-inline: %s guarded by zero-initialized "
		     "%s before initialization\n", cand.name (),
		     IDENTIFIER_POINTER (DECL_NAME (guard_decl)));
	  bb_guard_cache.put (target_bb, true);
	  return true;
	}
    }

  bb_guard_cache.put (target_bb, false);
  return false;
}

bool
guarded_load_prover::function_entry_safe_without_init_p
  (cgraph_node *node, auto_vec<cgraph_node *, 16> &stack)
{
  if (bool *cached = function_entry_safe_cache.get (node))
    return *cached;

  if (!closed_entry_p (node) || stack.contains (node))
    return false;

  auto_vec<cgraph_node *, 16> component;
  entry_cache.collect_component (node, component);

  for (unsigned i = 0; i < component.length (); ++i)
    if (!closed_entry_p (component[i]) || stack.contains (component[i]))
      return false;

  unsigned stack_len = stack.length ();
  for (unsigned i = 0; i < component.length (); ++i)
    stack.safe_push (component[i]);

  bool saw_caller = false;
  bool ok = true;
  for (unsigned i = 0; i < component.length () && ok; ++i)
    for (cgraph_edge *e = component[i]->callers; e; e = e->next_caller)
      {
	if (!e->caller || !e->call_stmt || e->indirect_inlining_edge)
	  {
	    ok = false;
	    break;
	  }

	if (component.contains (e->caller))
	  continue;

	saw_caller = true;
	if (!callsite_safe_without_init_p (e, stack))
	  {
	    ok = false;
	    break;
	  }
      }

  stack.truncate (stack_len);
  bool safe = saw_caller && ok;
  if (safe)
    for (unsigned i = 0; i < component.length (); ++i)
      function_entry_safe_cache.put (component[i], true);
  return safe;
}

bool
guarded_load_prover::callsite_safe_without_init_p
  (cgraph_edge *edge, auto_vec<cgraph_node *, 16> &stack)
{
  if (!edge)
    return false;

  if (bool *cached = callsite_safe_cache.get (edge))
    return *cached;

  if (!edge->caller || !edge->call_stmt)
    return false;

  entry_proof_tree_node *entry_tree = entry_cache.lookup (edge->caller);
  if (entry_tree && entry_tree->entry_status_known
      && entry_tree->entry_has_initialized)
    {
      callsite_safe_cache.put (edge, true);
      return true;
    }

  if (all_store_nodes_called_before_stmt_p (cand, edge->caller,
						   edge->call_stmt))
    {
      callsite_safe_cache.put (edge, true);
      return true;
    }

  basic_block call_bb = gimple_bb (edge->call_stmt);
  if (bb_guarded_by_false_condition_without_init_p (edge->caller, call_bb))
    {
      callsite_safe_cache.put (edge, true);
      return true;
    }

  bool entry_has_initialized;
  if (entry_tree && entry_tree->entry_status_known)
    entry_has_initialized = entry_tree->entry_has_initialized;
  else
    {
      auto_vec<cgraph_node *, 16> init_stack;
      entry_has_initialized
	= entry_cache.function_entry_has_initialized_p (edge->caller,
							init_stack);
    }
  if (entry_has_initialized)
    {
      callsite_safe_cache.put (edge, true);
      return true;
    }

  bool safe = function_entry_safe_without_init_p (edge->caller, stack);
  if (safe)
    callsite_safe_cache.put (edge, true);
  return safe;
}

bool
guarded_load_prover::load_safe (array_access &load)
{
  if (bool *cached = load_safe_cache.get (load.stmt))
    return *cached;

  if (bb_guarded_by_false_condition_without_init_p (load.node,
						    gimple_bb (load.stmt)))
    {
      load_safe_cache.put (load.stmt, true);
      return true;
    }

  auto_vec<cgraph_node *, 16> stack;
  bool safe = function_entry_safe_without_init_p (load.node, stack);
  load_safe_cache.put (load.stmt, safe);
  return safe;
}

static bool
simple_step_one_phi_p (gphi *phi, loop_p loop)
{
  edge latch = loop_latch_edge (loop);
  if (!latch)
    return false;

  tree next = PHI_ARG_DEF_FROM_EDGE (phi, latch);
  if (!next || TREE_CODE (next) != SSA_NAME)
    return false;

  gimple *def = SSA_NAME_DEF_STMT (next);
  if (!is_gimple_assign (def)
      || (gimple_assign_rhs_code (def) != PLUS_EXPR
	  && gimple_assign_rhs_code (def) != POINTER_PLUS_EXPR))
    return false;

  tree op0 = strip_ssa_copy (gimple_assign_rhs1 (def));
  tree op1 = gimple_assign_rhs2 (def);
  return op0 == gimple_phi_result (phi)
	 && TREE_CODE (op1) == INTEGER_CST
	 && tree_int_cst_equal (op1, integer_one_node);
}

/* Record every type in an integral copy/conversion chain.  This does not
   establish that the conversions preserve values: the complete IV range must
   fit *every* recorded type before the caller can use the underlying IV.
   In particular, a widening cast must not hide an earlier truncation.  */
static tree
index_before_conversions (tree index, auto_vec<tree, 8> &types)
{
  while (index && INTEGRAL_TYPE_P (TREE_TYPE (index)))
    {
      types.safe_push (TREE_TYPE (index));
      if (TREE_CODE (index) != SSA_NAME)
	break;
      gimple *def = SSA_NAME_DEF_STMT (index);
      if (!is_gimple_assign (def)
	  || (!gimple_assign_single_p (def)
	      && !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def))))
	break;
      tree rhs = gimple_assign_rhs1 (def);
      if (TREE_CODE (rhs) != SSA_NAME && TREE_CODE (rhs) != INTEGER_CST)
	break;
      index = rhs;
    }
  return index;
}

/* The recognized indices are nonnegative.  Requiring the largest value to
   fit each type makes every conversion an identity on the entire interval,
   including changes of signedness and safe narrowing conversions.  */
static bool
index_range_fits_types_p (unsigned high, const auto_vec<tree, 8> &types)
{
  widest_int max_index = high;
  for (tree type : types)
    if (!wi::fits_to_tree_p (max_index, type))
      return false;
  return true;
}

static bool
simple_iv_range (tree index, gimple *use_stmt, unsigned &low, unsigned &high,
		 loop_p &source_loop)
{
  source_loop = nullptr;
  auto_vec<tree, 8> types;
  index = index_before_conversions (index, types);
  if (!index || !INTEGRAL_TYPE_P (TREE_TYPE (index)))
    return false;
  if (tree_uhwi_p (index, low))
    {
      high = low;
      return index_range_fits_types_p (high, types);
    }

  if (TREE_CODE (index) != SSA_NAME)
    return false;

  gimple *def = SSA_NAME_DEF_STMT (index);
  if (!def || gimple_code (def) != GIMPLE_PHI)
    return false;

  gphi *phi = as_a<gphi *> (def);
  basic_block header = gimple_bb (phi);
  if (!current_loops)
    return false;

  loop_p loop = header ? header->loop_father : nullptr;
  if (!loop || loop == current_loops->tree_root || header != loop->header)
    return false;

  if (!flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
    return false;

  edge preheader = loop_preheader_edge (loop);
  if (!preheader)
    return false;

  tree init = PHI_ARG_DEF_FROM_EDGE (phi, preheader);
  unsigned init_value;
  if (!tree_uhwi_p (init, init_value) || init_value != 0)
    return false;

  if (!simple_step_one_phi_p (phi, loop))
    return false;

  tree niters = number_of_latch_executions (loop);
  unsigned niter_value;
  if (!tree_uhwi_p (niters, niter_value) || niter_value == 0)
    return false;

  low = 0;
  high = niter_value - 1;
  if (!index_range_fits_types_p (high, types))
    return false;
  source_loop = loop;
  return true;
}

static bool
access_ranges_p (array_candidate &cand, array_access &access)
{
  access.lows.truncate (0);
  access.highs.truncate (0);
  access.index_loops.truncate (0);

  for (unsigned i = 0; i < access.indices.length (); ++i)
    {
      unsigned low, high;
      loop_p source_loop;
      if (!simple_iv_range (access.indices[i], access.stmt, low, high,
			    source_loop))
	return false;

      if (high >= cand.dims[i])
	return false;

      access.lows.safe_push (low);
      access.highs.safe_push (high);
      access.index_loops.safe_push (source_loop);
    }

  if (cand.dims.length () > 1)
    {
      auto_vec<loop_p, 4> varying_loops;
      for (unsigned i = 0; i < access.index_loops.length (); ++i)
	{
	  if (access.lows[i] == access.highs[i])
	    continue;

	  loop_p loop = access.index_loops[i];
	  if (!loop)
	    return false;

	  for (unsigned j = 0; j < varying_loops.length (); ++j)
	    {
	      loop_p seen = varying_loops[j];
	      if (seen == loop || seen->num == loop->num)
		return false;

	      if (!loop_contains_p (seen, loop)
		  && !loop_contains_p (loop, seen))
		return false;
	    }

	  varying_loops.safe_push (loop);
	}
    }

  return true;
}

static bool
simple_code_p (tree_code code)
{
  switch (code)
    {
    case INTEGER_CST:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_NOT_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case NEGATE_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
    case NON_LVALUE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
      return true;

    default:
      return false;
    }
}

class formula_expander
{
public:
  /* A null SEQ validates the formula without constructing expressions.
     Otherwise each expanded node is materialized once in SEQ, preserving
     the sharing of the source SSA DAG through GIMPLE generation.  */
  formula_expander (const vec<tree> &formals, const vec<tree> &actuals,
		    gimple_seq *seq = nullptr)
    : formals (formals), actuals (actuals), seq (seq)
  {}

  tree expand (tree expr);

  const char *failure () const { return reason ? reason : ""; }

private:
  tree expand_1 (tree);
  tree expand_assign (gimple *);
  void fail (const char *msg)
  {
    if (!reason)
      reason = msg;
  }

  const vec<tree> &formals;
  const vec<tree> &actuals;
  gimple_seq *seq;
  const char *reason = nullptr;
  hash_set<tree> visiting;
  hash_map<tree, tree> expanded;
  unsigned depth = 0;
  unsigned nodes = 0;
};

tree
formula_expander::expand_assign (gimple *stmt)
{
  tree_code code = gimple_assign_rhs_code (stmt);
  if (code == COND_EXPR)
    {
      fail ("complex expressions are not supported");
      return nullptr;
    }
  else if (!simple_code_p (code)
	   && gimple_assign_rhs_class (stmt) != GIMPLE_SINGLE_RHS)
    {
      fail ("unsupported expression");
      return nullptr;
    }

  switch (gimple_assign_rhs_class (stmt))
    {
    case GIMPLE_SINGLE_RHS:
      return expand (gimple_assign_rhs1 (stmt));

    case GIMPLE_UNARY_RHS:
      {
	tree op0 = expand (gimple_assign_rhs1 (stmt));
	if (!op0)
	  return nullptr;
	if (!seq)
	  return gimple_assign_lhs (stmt);
	return fold_build1 (code, TREE_TYPE (gimple_assign_lhs (stmt)), op0);
      }

    case GIMPLE_BINARY_RHS:
      {
	tree op0 = expand (gimple_assign_rhs1 (stmt));
	tree op1 = expand (gimple_assign_rhs2 (stmt));
	if (!op0 || !op1)
	  return nullptr;
	if (!seq)
	  return gimple_assign_lhs (stmt);
	return fold_build2 (code, TREE_TYPE (gimple_assign_lhs (stmt)), op0,
			    op1);
      }

    case GIMPLE_TERNARY_RHS:
      {
	tree op0 = expand (gimple_assign_rhs1 (stmt));
	tree op1 = expand (gimple_assign_rhs2 (stmt));
	tree op2 = expand (gimple_assign_rhs3 (stmt));
	if (!op0 || !op1 || !op2)
	  return nullptr;
	if (!seq)
	  return gimple_assign_lhs (stmt);
	return fold_build3 (code, TREE_TYPE (gimple_assign_lhs (stmt)),
			    op0, op1, op2);
      }

    default:
      fail ("unsupported expression");
      return nullptr;
    }
}

tree
formula_expander::expand (tree expr)
{
  if (!expr || reason)
    return nullptr;
  if (tree *cached = expanded.get (expr))
    return *cached;

  /* Resource limits, independent of profitability: bound recursion and
     generated IR even for unusually large input expressions.  */
  if (depth >= 256 || nodes >= 4096)
    {
      fail ("formula exceeds analysis limits");
      return nullptr;
    }
  ++depth;
  ++nodes;
  tree value = expand_1 (expr);
  --depth;
  if (value && seq)
    {
      gimple_seq stmts = NULL;
      value = force_gimple_operand (value, &stmts, true, NULL_TREE);
      gimple_seq_add_seq (seq, stmts);
    }
  if (value)
    expanded.put (expr, value);
  return value;
}

tree
formula_expander::expand_1 (tree expr)
{
  if (!expr || reason)
    return nullptr;

  for (unsigned i = 0; i < formals.length (); ++i)
    if (operand_equal_p (expr, formals[i]))
      return seq ? fold_convert (TREE_TYPE (expr), actuals[i]) : expr;

  tree_code code = TREE_CODE (expr);
  if (TREE_CODE_CLASS (code) == tcc_constant)
    return seq ? unshare_expr (expr) : expr;

  if (code == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (expr))
	{
	  fail ("formula uses non-index value");
	  return nullptr;
	}

      if (visiting.add (expr))
	{
	  fail ("cyclic formula");
	  return nullptr;
	}

      gimple *stmt = SSA_NAME_DEF_STMT (expr);
      if (!stmt || !is_gimple_assign (stmt))
	{
	  fail ("formula uses unsupported SSA definition");
	  return nullptr;
	}

      tree value = expand_assign (stmt);
      visiting.remove (expr);
      return value;
    }

  if (code == COND_EXPR)
    {
      fail ("complex expressions are not supported");
      return nullptr;
    }
  else if (!simple_code_p (code))
    {
      fail ("unsupported expression");
      return nullptr;
    }

  switch (TREE_CODE_LENGTH (code))
    {
    case 1:
      {
	tree op0 = expand (TREE_OPERAND (expr, 0));
	if (!op0)
	  return nullptr;
	if (!seq)
	  return expr;
	return fold_build1 (code, TREE_TYPE (expr), op0);
      }

    case 2:
      {
	tree op0 = expand (TREE_OPERAND (expr, 0));
	tree op1 = expand (TREE_OPERAND (expr, 1));
	if (!op0 || !op1)
	  return nullptr;
	if (!seq)
	  return expr;
	return fold_build2 (code, TREE_TYPE (expr), op0, op1);
      }

    case 3:
      {
	tree op0 = expand (TREE_OPERAND (expr, 0));
	tree op1 = expand (TREE_OPERAND (expr, 1));
	tree op2 = expand (TREE_OPERAND (expr, 2));
	if (!op0 || !op1 || !op2)
	  return nullptr;
	if (!seq)
	  return expr;
	return fold_build3 (code, TREE_TYPE (expr), op0, op1, op2);
      }

    default:
      fail ("unsupported expression");
      return nullptr;
    }
}

static bool
full_region_p (array_candidate &cand, array_access &region)
{
  for (unsigned i = 0; i < cand.dims.length (); ++i)
    if (region.lows[i] != 0 || region.highs[i] != cand.dims[i] - 1)
      return false;

  return true;
}

static bool
validate_formula (array_candidate &cand)
{
  array_access &store = *cand.stores[0];
  formula_expander validator (store.indices, store.indices);
  if (!validator.expand (store.rhs))
    {
      cand.reject_reason = validator.failure ();
      return false;
    }
  return true;
}

static tree
build_generic_formula (array_candidate &cand, const vec<tree> &load_indices,
		       gimple_seq *seq)
{
  array_access &store = *cand.stores[0];
  formula_expander expander (store.indices, load_indices, seq);
  return expander.expand (store.rhs);
}

class array_load_inliner
{
public:
  unsigned execute ();

private:
  bool collect_accesses (array_candidate &);
  bool collect_accesses_in_node (cgraph_node *, array_candidate &,
				 const auto_vec<ipa_ref *, 32> &);
  bool collect_stmt_accesses (cgraph_node *, gimple *, array_candidate &);
  bool prove_closed_world (array_candidate &);
  bool prove_coverage (array_candidate &);
  bool all_stores_dominate_load_p (array_candidate &, array_access &,
				   dominance_failure &);
  bool prove_dominance (array_candidate &);
  bool prove_guarded_loads (array_candidate &);
  void rewrite_load (array_candidate &, array_access &);
  bool store_removable_p (array_candidate &, array_access *);
  void remove_store (array_candidate &, array_access *);
  bool remove_stores (array_candidate &);
  void process_candidate (varpool_node *);

  function_analysis_cache analyses;
  bool changed = false;
  bool removed_stores = false;
  dominance_failure dom_failure;
  entry_proof_cache entry_cache;
};

bool
array_load_inliner::collect_stmt_accesses (cgraph_node *node, gimple *stmt,
					   array_candidate &cand)
{
  if (gimple_has_volatile_ops (stmt)
      && (expr_contains_decl_p (gimple_get_lhs (stmt), cand.decl)
	  || (is_gimple_assign (stmt)
	      && expr_contains_decl_p (gimple_assign_rhs1 (stmt),
				       cand.decl))))
    {
      cand.reject_reason = "volatile access";
      return false;
    }

  if (is_gimple_assign (stmt))
    {
      tree rhs = gimple_assign_rhs1 (stmt);
      tree lhs = gimple_get_lhs (stmt);
      tree ref_decl = nullptr;
      auto_vec<tree> indices;
      bool lhs_ref = lhs && extract_array_ref (lhs, ref_decl, indices)
		     && ref_decl == cand.decl;
      bool rhs_ref = rhs && extract_array_ref (rhs, ref_decl, indices)
		     && ref_decl == cand.decl;

      if ((lhs && expr_contains_decl_p (lhs, cand.decl) && !lhs_ref)
	  || (rhs && expr_contains_decl_p (rhs, cand.decl) && !rhs_ref)
	  || (gimple_num_ops (stmt) > 2
	      && expr_contains_decl_p (gimple_op (stmt, 2), cand.decl)))
	{
	  cand.reject_reason = "address escapes";
	  return false;
	}

      if (lhs_ref)
	{
	  if (indices.length () != cand.dims.length ()
	      || expr_contains_decl_p (rhs, cand.decl))
	    {
	      cand.reject_reason = "address escapes";
	      return false;
	    }

	  array_access *access
	    = cand.record_access (node, stmt, true, rhs, indices);
	  if (!access_ranges_p (cand, *access))
	    {
	      cand.reject_reason = "not fully initialized";
	      return false;
	    }
	}
      else if (rhs_ref)
	{
	  if (!gimple_assign_single_p (stmt)
	      || indices.length () != cand.dims.length ())
	    {
	      cand.reject_reason = "address escapes";
	      return false;
	    }

	  cand.record_access (node, stmt, false, rhs, indices);
	}

      return true;
    }

  for (unsigned i = 0; i < gimple_num_ops (stmt); ++i)
    if (expr_contains_decl_p (gimple_op (stmt, i), cand.decl))
      {
	cand.reject_reason = "unknown call side effects or address escapes";
	return false;
      }

  return true;
}

bool
array_load_inliner::collect_accesses_in_node (cgraph_node *node,
					      array_candidate &cand,
					      const auto_vec<ipa_ref *, 32> &refs)
{
  scoped_dump_file no_dump;
  bool has_store = false;
  for (ipa_ref *ref : refs)
    if (ref->use == IPA_REF_STORE)
      has_store = true;
  cfun_saver save (cand.analyses, node,
		  has_store ? LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS : 0);
  scev_context scev (has_store);

  hash_set<gimple *> seen_stmts;
  for (unsigned i = 0; i < refs.length (); ++i)
    {
      ipa_ref *ref = refs[i];
      gcc_assert (ref->referring == node);
      if (seen_stmts.add (ref->stmt))
	continue;
      if (!collect_stmt_accesses (node, ref->stmt, cand))
	return false;
    }

  return true;
}

bool
array_load_inliner::collect_accesses (array_candidate &cand)
{
  if (TREE_THIS_VOLATILE (cand.decl))
    {
      cand.reject_reason = "volatile access";
      return false;
    }

  if (!cand.var->all_refs_explicit_p ())
    {
      cand.reject_reason = "array references are not fully explicit";
      return false;
    }

  hash_map<cgraph_node *, auto_vec<ipa_ref *, 32> *> refs_by_node;
  auto_delete_vec<auto_vec<ipa_ref *, 32>> grouped_refs;
  unsigned ref_count = 0;
  ipa_ref *ref = nullptr;
  for (unsigned i = 0; cand.var->iterate_referring (i, ref); ++i)
    {
      if (!ref->stmt || ref->use == IPA_REF_ALIAS)
	{
	  cand.reject_reason = "address escapes";
	  return false;
	}

      cgraph_node *node = dyn_cast<cgraph_node *> (ref->referring);
      if (!visible_node_with_body_p (node))
	{
	  cand.reject_reason = "unknown call side effects or address escapes";
	  return false;
	}

      auto_vec<ipa_ref *, 32> *&refs = refs_by_node.get_or_insert (node);
      if (!refs)
	{
	  refs = new auto_vec<ipa_ref *, 32>;
	  grouped_refs.safe_push (refs);
	}
      refs->safe_push (ref);
      ++ref_count;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "array-load-inline: collect %s: scanning %u reference stmts "
	     "in %u functions\n", cand.name (), ref_count,
	     grouped_refs.length ());

  for (auto refs : grouped_refs)
    {
      cgraph_node *node = as_a<cgraph_node *> ((*refs)[0]->referring);
      if (!collect_accesses_in_node (node, cand, *refs))
	return false;
    }

  return true;
}

static bool
load_function_closed_world_p (cgraph_node *node)
{
  return closed_entry_p (node);
}

bool
array_load_inliner::prove_closed_world (array_candidate &cand)
{
  if (!cand.var->all_refs_explicit_p ())
    {
      cand.reject_reason = "array references are not fully explicit";
      return false;
    }

  auto_vec<cgraph_node *, 8> seen_load_nodes;
  for (unsigned i = 0; i < cand.loads.length (); ++i)
    {
      cgraph_node *node = cand.loads[i]->node;
      if (seen_load_nodes.contains (node))
	continue;

      seen_load_nodes.safe_push (node);
      if (!load_function_closed_world_p (node))
	{
	  cand.reject_reason = "load function is externally reachable";
	  dump_unanalyzable_node_details (node);
	  return false;
	}
    }

  return true;
}

bool
array_load_inliner::prove_coverage (array_candidate &cand)
{
  if (cand.stores.length () != 1)
    {
      cand.reject_reason = cand.stores.is_empty ()
			   ? "not fully initialized"
			   : "multiple store regions are not supported";
      return false;
    }

  array_access &store = *cand.stores[0];
  if (store.lows.length () != cand.dims.length ()
      || !full_region_p (cand, store))
    {
      cand.reject_reason = "not fully initialized";
      return false;
    }

  if (dump_file)
    fprintf (dump_file,
	     "array-load-inline: %s initialization coverage complete\n",
	     cand.name ());
  return true;
}

bool
array_load_inliner::all_stores_dominate_load_p (array_candidate &cand,
						array_access &load,
						dominance_failure &failure)
{
  if (bool *cached = cand.load_dominance.get (load.stmt))
    return *cached;

  auto_vec<cgraph_node *, 16> stack;
  entry_proof_tree_node *entry = entry_cache.lookup (load.node);
  bool initialized
    = (entry && entry->entry_status_known && entry->entry_has_initialized)
      || all_store_nodes_called_before_stmt_p (cand, load.node, load.stmt)
      || entry_cache.function_entry_has_initialized_p (load.node, stack);
  cand.load_dominance.put (load.stmt, initialized);
  if (!initialized)
    failure.set_at_load (load,
			 "initialization does not dominate load statement");
  return initialized;
}

bool
array_load_inliner::prove_dominance (array_candidate &cand)
{
  dom_failure.clear ();
  for (array_access *load : cand.loads)
    if (!all_stores_dominate_load_p (cand, *load, dom_failure))
      {
	cand.reject_reason = "init does not dominate load";
	return false;
      }
  return true;
}

bool
array_load_inliner::prove_guarded_loads (array_candidate &cand)
{
  guarded_load_prover prover (cand, entry_cache);
  for (unsigned i = 0; i < cand.loads.length (); ++i)
    {
      array_access *load = cand.loads[i];
      dominance_failure ignored_failure;
      bool stores_dominate = all_stores_dominate_load_p (cand, *load,
							 ignored_failure);
      if (stores_dominate)
	continue;

      if (!prover.load_safe (*load))
	{
	  cand.reject_reason = "init does not dominate load";
	  return false;
	}

      if (dump_file)
	{
	  fprintf (dump_file,
		   "array-load-inline: load of %s is unreachable before "
		   "initialization: ", cand.name ());
	  print_gimple_stmt (dump_file, load->stmt, 0, TDF_SLIM);
	}
    }

  if (dump_file)
    fprintf (dump_file,
	     "array-load-inline: %s loads are either strictly dominated by "
	     "initialization or guarded before initialization\n", cand.name ());

  return true;
}

void
array_load_inliner::rewrite_load (array_candidate &cand, array_access &load)
{
  cgraph_node *node = load.node;
  cfun_saver save (cand.analyses, node);
  gimple *stmt = load.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gimple_seq seq = NULL;
  tree value = build_generic_formula (cand, load.indices, &seq);
  gcc_assert (value);
  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
  gimple *new_stmt = gimple_build_assign (gimple_assign_lhs (stmt), value);
  gimple_set_location (new_stmt, gimple_location (stmt));

  if (dump_file)
    {
      fprintf (dump_file, "array-load-inline: inlined load of %s in %s: ",
	       cand.name (), node->name ());
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  node->remove_stmt_references (stmt);
  gsi_replace (&gsi, new_stmt, true);
  node->record_stmt_references (new_stmt);
  changed = true;
}

bool
array_load_inliner::store_removable_p (array_candidate &cand,
				       array_access *store)
{
  cgraph_node *node = store->node;
  if (!visible_node_with_body_p (node))
    return false;

  cfun_saver save (cand.analyses, node);

  gimple_stmt_iterator gsi = gsi_for_stmt (store->stmt);
  gimple *stmt = gsi_stmt (gsi);
  tree ref_decl = nullptr;
  auto_vec<tree> indices;
  return stmt && is_gimple_assign (stmt)
	 && extract_array_ref (gimple_get_lhs (stmt), ref_decl, indices)
	 && ref_decl == cand.decl;
}

void
array_load_inliner::remove_store (array_candidate &cand, array_access *store)
{
  cgraph_node *node = store->node;
  cfun_saver save (cand.analyses, node);

  gimple_stmt_iterator gsi = gsi_for_stmt (store->stmt);
  gimple *stmt = gsi_stmt (gsi);

  if (dump_file)
    {
      fprintf (dump_file, "array-load-inline: removed dead store to %s in %s: ",
	       cand.name (), node->name ());
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  node->remove_stmt_references (stmt);
  unlink_stmt_vdef (stmt);
  gsi_remove (&gsi, true);
  release_defs (stmt);
  changed = true;
  removed_stores = true;
}

bool
array_load_inliner::remove_stores (array_candidate &cand)
{
  if (!cand.var->all_refs_explicit_p () || !cand.var->can_remove_if_no_refs_p ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "array-load-inline: keep stores to %s because references are "
		 "not fully removable\n", cand.name ());
      return false;
    }

  for (unsigned i = 0; i < cand.stores.length (); ++i)
    if (!store_removable_p (cand, cand.stores[i]))
      return false;

  for (unsigned i = 0; i < cand.stores.length (); ++i)
    remove_store (cand, cand.stores[i]);

  return !cand.stores.is_empty ();
}

void
array_load_inliner::process_candidate (varpool_node *var)
{
  array_candidate cand (analyses);

  if (!cand.init (var))
    return;

  entry_cache.set_candidate (cand);
  dom_failure.clear ();

  if (!collect_accesses (cand))
    {
      if (cand.reject_reason
	  && strcmp (cand.reject_reason,
		     "array references are not fully explicit") == 0
	  && candidate_has_explicit_load_ref_p (cand))
	dump_candidate (cand.decl, cand.dims);
      dump_candidate_reject (cand);
      return;
    }

  if (cand.loads.is_empty ())
    return;

  dump_candidate (cand.decl, cand.dims);

  if (!prove_coverage (cand) || !prove_closed_world (cand)
      || !validate_formula (cand))
    {
      dump_candidate_reject (cand);
      return;
    }

  if (!prove_dominance (cand) && !prove_guarded_loads (cand))
    {
      dump_dominance_failure (cand, dom_failure, entry_cache);
      dump_candidate_reject (cand);
      return;
    }

  for (array_access *load : cand.loads)
    rewrite_load (cand, *load);

  if (remove_stores (cand))
    return;

  if (!dump_file)
    return;

  fprintf (dump_file,
	   "array-load-inline: symbol %s not removed in this pass; later "
	   "DCE/varpool cleanup must remove it if no references remain\n",
	   cand.name ());
}

unsigned
array_load_inliner::execute ()
{
  {
    scoped_dump_file no_dump;
    cgraph_node *node = nullptr;
    FOR_EACH_FUNCTION (node)
      if (visible_node_with_body_p (node))
	node->get_body ();
  }

  varpool_node *var = nullptr;
  FOR_EACH_VARIABLE (var)
    process_candidate (var);

  return changed ? (TODO_update_ssa
		    | (removed_stores ? TODO_remove_functions : 0)) : 0;
}

const pass_data pass_data_ipa_array_load_inline =
{
  SIMPLE_IPA_PASS, /* type */
  "array-load-inline", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_ARRAY_LOAD_INLINE, /* tv_id */
  PROP_cfg | PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_array_load_inline : public simple_ipa_opt_pass
{
public:
  pass_ipa_array_load_inline (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_array_load_inline, ctxt)
  {}

  bool gate (function *) final override
  {
    return optimize >= 3 && flag_ipa_array_load_inline;
  }

  unsigned int execute (function *) final override
  {
    return array_load_inliner ().execute ();
  }
};

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_array_load_inline (gcc::context *ctxt)
{
  return new pass_ipa_array_load_inline (ctxt);
}
