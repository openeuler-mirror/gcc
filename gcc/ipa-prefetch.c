/* IPA prefetch optimizations.
   Copyright (C) 2023 Free Software Foundation, Inc.
   Contributed by Ilia Diachkov.

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

/* IPA prefetch is an interprocedural pass that detects cases of indirect
   memory access potentially in loops and inserts prefetch instructions
   to optimize cache usage during these indirect memory accesses.  */

#include "config.h"
#define INCLUDE_SET
#define INCLUDE_MAP
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tm.h"
#include "tree.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "vec.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "cfg.h"
#include "cfghooks.h"
#include "ssa.h"
#include "tree-dfa.h"
#include "fold-const.h"
#include "tree-inline.h"
#include "stor-layout.h"
#include "tree-into-ssa.h"
#include "tree-cfg.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "tree-eh.h"
#include "bitmap.h"
#include "cfgloop.h"
#include "langhooks.h"
#include "ipa-param-manipulation.h"
#include "ipa-fnsummary.h"
#include "tree-ssa-loop.h"
#include "tree-ssa-loop-ivopts.h"
#include "gimple-fold.h"
#include "gimplify.h"

namespace {

/* Call graph analysis.  */

typedef std::set<cgraph_edge *> edge_set;
typedef std::set<cgraph_node *> node_set;
typedef std::map<cgraph_node *, edge_set *> node_to_iedge_map;
typedef std::map<cgraph_node *, node_set *> node_to_node_map;
typedef std::map<cgraph_edge *, double> edge_in_loop;
typedef std::map<cgraph_node *, double> node_in_loop;

static edge_in_loop *el_map = NULL;
static node_in_loop *nl_map = NULL;
static node_to_iedge_map *icn_map = NULL;
/* Contains nodes which reachable from a given node.  */
static node_to_node_map *nn_map = NULL;

static bool
can_be_optimized (cgraph_node *n)
{
  /* TODO: maybe check also inlined_to.  */
  return opt_for_fn (n->decl, flag_ipa_prefetch) && n->has_gimple_body_p ();
}

static void
analyze_cgraph_edge (cgraph_edge *e)
{
  gcall *stmt = e->call_stmt;
  gcc_checking_assert (e && stmt);
  basic_block bb = gimple_bb (stmt);
  gcc_checking_assert (bb);
  /* TODO: add the same check for indirect calls.  */
  if (e->callee && !can_be_optimized (e->callee))
    return;

  if (dump_file)
    {
      if (e->callee)
	fprintf (dump_file, "\t%*s%s %s%*s  ", 1, "",
		 e->callee->dump_name (), !e->inline_failed ? "inlined" :
		 cgraph_inline_failed_string (e->inline_failed), 1, "");
      else
	fprintf (dump_file, "\t%*s%s %s%*s  ", 1, "", "(indirect)",
		 "n/a", 1, "");
      fprintf (dump_file, "freq:%4.2f", e->sreal_frequency ().to_double ());

      if (e->callee && cross_module_call_p (e))
	fprintf (dump_file, " cross module");

      class ipa_call_summary *es = ipa_call_summaries->get (e);
      if (es)
	fprintf (dump_file, " loop depth:%2i size:%2i time: %2i",
		 es->loop_depth, es->call_stmt_size, es->call_stmt_time);

      fprintf (dump_file, "\n");
    }
  if (e->indirect_info && dump_file)
    {
      fprintf (dump_file, "II: %p\n", (void *) e->indirect_info->targets);
      unsigned i = 0;
      cgraph_node *n;
      if (e->indirect_info->targets)
	for (i = 0; e->indirect_info->targets->iterate (i, &n); ++i)
	  fprintf (dump_file, "\t%s\n", n->dump_name ());
    }

  if (bb_loop_depth (bb) == 0)
    return;

  if (dump_file)
    {
      if (e->callee)
	fprintf (dump_file, "\tCall in loop (%d): ", bb_loop_depth (bb));
      else
	fprintf (dump_file, "\tICall in loop (%d): ", bb_loop_depth (bb));
      print_gimple_stmt (dump_file, stmt, 0);
    }
  (*el_map)[e] = e->sreal_frequency ().to_double ();
}

/* Walk optimizible cgraph nodes and collect info for edges.  */

static void
analyse_cgraph ()
{
  cgraph_node *n;
  cgraph_edge *e;
  FOR_EACH_DEFINED_FUNCTION (n)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\n\nProcesing function %s\n", n->dump_name ());
	  print_generic_expr (dump_file, n->decl);
	  fprintf (dump_file, "\n");
	}
      if (!can_be_optimized (n))
	{
	  if (dump_file)
	    fprintf (dump_file, "Skip the function\n");
	  continue;
	}

      /* TODO: maybe remove loop info here.  */
      n->get_body ();
      push_cfun (DECL_STRUCT_FUNCTION (n->decl));
      calculate_dominance_info (CDI_DOMINATORS);
      loop_optimizer_init (LOOPS_NORMAL);

      for (e = n->callees; e; e = e->next_callee)
	analyze_cgraph_edge (e);
      for (e = n->indirect_calls; e; e = e->next_callee)
	analyze_cgraph_edge (e);

      free_dominance_info (CDI_DOMINATORS);
      loop_optimizer_finalize ();

      pop_cfun ();
    }
}

/* Save indirect call info to node:icall_target map.  */

static void
prepare_indirect_call_info ()
{
  cgraph_node *n, *n2;
  cgraph_edge *e;
  FOR_EACH_DEFINED_FUNCTION (n)
    for (e = n->indirect_calls; e; e = e->next_callee)
      {
	if (!e->indirect_info->targets)
	  continue;
	for (unsigned i = 0; e->indirect_info->targets->iterate (i, &n2); ++i)
	  {
	    if (icn_map->count (n2) == 0)
	      (*icn_map)[n2] = new edge_set;
	    (*icn_map)[n2]->insert (e);
	  }
      }
}

static void
collect_nn_info (struct cgraph_edge *e, struct cgraph_node *n)
{
  struct cgraph_node *n2 = e->caller;
  if (nn_map->count (n2) == 0)
    (*nn_map)[n2] = new node_set;
  (*nn_map)[n2]->insert (n);
  if (nn_map->count (n) != 0)
    {
      node_set *set = (*nn_map)[n];
      for (node_set::const_iterator it = set->begin ();
	   it != set->end (); it++)
	(*nn_map)[n2]->insert (*it);
    }
}

static bool
check_loop_info_for_cgraph_edge (struct cgraph_edge *e, struct cgraph_node *n,
				 bool &all_in_loop, double &rate)
{
  collect_nn_info (e, n);
  if (el_map->count (e) == 0)
    {
      if (dump_file)
	fprintf (dump_file, "not all: %s->%s\n",
		 e->caller->dump_name (), n->dump_name ());
      all_in_loop = false;
      return false;
    }
  rate += (*el_map)[e];
  return true;
}

static bool
update_loop_info_for_cgraph_node (struct cgraph_node *n)
{
  bool changed = false, all_in_loop = true;
  double rate = 0.0;
  struct cgraph_edge *e;

  /* Iterate all direct callers.  */
  if (n->callers)
    for (e = n->callers; e; e = e->next_caller)
      if (!check_loop_info_for_cgraph_edge (e, n, all_in_loop, rate))
	break;

  /* Iterate all possible indirect callers.  */
  edge_set *set = (*icn_map)[n];
  if (set)
    for (edge_set::const_iterator it = set->begin (); it != set->end (); it++)
      if (!check_loop_info_for_cgraph_edge (*it, n, all_in_loop, rate))
	break;

  /* The node had 0 loop count but the rate is > 0,
     so something is changed.  */
  if (dump_file)
    fprintf (dump_file, "%s: all=%d, nl->c=%lu, r=%4.2f\n", n->dump_name (),
	     all_in_loop, nl_map->count (n), rate);

  if (all_in_loop && nl_map->count (n) == 0 && rate > 0.0)
    {
      if (dump_file)
	fprintf (dump_file, "%s: new rate %4.2f\n", n->dump_name (), rate);
      changed = true;
    }
  if (all_in_loop)
    {
      (*nl_map)[n] = nl_map->count (n) ? (*nl_map)[n] + rate : rate;
      for (e = n->callees; e; e = e->next_callee)
	(*el_map)[e] = el_map->count (e) ? (*el_map)[e] + rate : rate;
      for (e = n->indirect_calls; e; e = e->next_callee)
	{
	  (*el_map)[e] = el_map->count (e) ? (*el_map)[e] + rate : rate;
	  if (dump_file)
	    fprintf (dump_file, "%s: reset indirect e=%p to %4.2f\n",
		     n->dump_name (), (void *) e, (*el_map)[e]);
	}
    }
  return changed;
}

/* Propagate in_loop info over the call graph.  */

static void
propagate_loop_info_in_cgraph ()
{
  struct cgraph_node *n;
  bool changed;
  unsigned iteration = 0;
  do
    {
      changed = false;
      if (dump_file)
	fprintf (dump_file, "\nIteration %u\n", iteration++);
      FOR_EACH_DEFINED_FUNCTION (n)
	{
	  if (!n->callers && !(*icn_map)[n])
	    continue;
	  if (update_loop_info_for_cgraph_node (n))
	    changed = true;
	}
  } while (changed);

  if (dump_file)
    {
      fprintf (dump_file, "\nList of nodes in loops:\n");
      FOR_EACH_DEFINED_FUNCTION (n)
	if (nl_map->count (n) != 0)
	  fprintf (dump_file, "%s: %4.2f\n", n->dump_name (), (*nl_map)[n]);
      fprintf (dump_file, "\nList of callable nodes:\n");
      FOR_EACH_DEFINED_FUNCTION (n)
	if (nn_map->count (n) != 0)
	  {
	    node_set *set = (*nn_map)[n];
	    fprintf (dump_file, "%s: ", n->dump_name ());
	    for (node_set::const_iterator it = set->begin ();
		 it != set->end (); it++)
	      fprintf (dump_file, "%s ", (*it)->dump_name ());
	    fprintf (dump_file, "\n");
	  }
    }
}

/* Analysis of memory references.  */

typedef enum
{
  MR_NONE,
  MR_SIMPLE,
  MR_POLYNOMIAL,
  MR_INDIRECT,
  MR_UNSUPPORTED
} mr_type;
const char *mr_type_str[] =
    {"none", "simple", "poly", "indirect", "unsuppoted"};

struct memref_type;
typedef std::set<memref_type *> memref_set;

static unsigned max_mr_id = 0;
typedef struct memref_type
{
  unsigned mr_id = 0;
  mr_type type = MR_NONE;
  tree mem = NULL_TREE;
  tree base = NULL_TREE;
  tree offset = NULL_TREE;
  vec<gimple *, va_heap, vl_ptr> stmts = vNULL;
  memref_set used_mrs;
  bool is_store = false;
  bool is_incr = false;
  tree step = NULL_TREE;
} memref_t;

typedef std::map<tree, memref_t *> tree_memref_map;
typedef std::map<function *, vec<memref_t *> > function_mrs_map;
typedef std::map<function *, memref_set *> funct_mrs_map;
typedef std::map<memref_t *, memref_t *> memref_map;
typedef std::map<memref_t *, tree> memref_tree_map;

typedef std::set<gimple *> stmt_set;
typedef std::map<tree, tree> tree_map;

tree_memref_map *tm_map;
funct_mrs_map *fmrs_map;
funct_mrs_map *optimize_mrs_map;
memref_map *mr_candidate_map;
tree_map *decl_map;

static void analyse_mem_ref (gimple *stmt, tree mem, memref_t* mr);

static memref_t*
get_memref (gimple *stmt, tree mem, bool is_store)
{
  if (tm_map->count (mem))
    {
      if (dump_file)
	fprintf (dump_file, "Found mr %d for %p.\n",
		 (*tm_map)[mem]->mr_id, (void *) mem);
      return (*tm_map)[mem];
    }

  memref_t *mr = new memref_t;
  mr->mr_id = ++max_mr_id;
  mr->is_store = is_store;
  mr->mem = mem;
  (*tm_map)[mem] = mr;
  if (dump_file)
    fprintf (dump_file, "Create mr %d for %p.\n",
	     mr->mr_id, (void *) mem);
  analyse_mem_ref (stmt, mem, mr);
  return mr;
}

static void
print_mrs_ids (memref_set &mrs, const char *start)
{
  if (start)
    fprintf (dump_file, "%s", start);
  for (memref_set::const_iterator it = mrs.begin (); it != mrs.end (); it++)
    fprintf (dump_file, "%d ", (*it)->mr_id);
  fprintf (dump_file, "\n");
}

static void
print_memref (memref_t *mr)
{
  fprintf (dump_file, "MR (%d) type: %s (%s) mem: ", mr->mr_id,
	   mr_type_str[mr->type], mr->is_store ? "st" : "ld");
  print_generic_expr (dump_file, mr->mem);
  fprintf (dump_file, "\nbase: ");
  if (mr->base)
    print_generic_expr (dump_file, mr->base);
  else
    fprintf (dump_file, "null");
  fprintf (dump_file, "\noffset: ");
  if (mr->offset)
    print_generic_expr (dump_file, mr->offset);
  else
    fprintf (dump_file, "null");
  fprintf (dump_file, "\nstmts:\n");
  for (unsigned int i = 0; i < mr->stmts.length (); i++)
    print_gimple_stmt (dump_file, mr->stmts[i], 0);
  print_mrs_ids (mr->used_mrs, "\tused memrefs: ");
  if (mr->is_incr)
    {
      fprintf (dump_file, "\tis incremental with step: ");
      print_generic_expr (dump_file, mr->step);
    }
  fprintf (dump_file, "\n");
}

/* If there is a simple load or store to a memory reference in STMT, returns
   the location of the memory reference, and sets IS_STORE according to whether
   it is a store or load.  Otherwise, returns NULL.
   TODO: from gcc/tree-ssa-loop-im.c, maybe make it global.  */

static tree *
simple_mem_ref_in_stmt (gimple *stmt, bool *is_store)
{
  tree *lhs, *rhs;

  /* Recognize SSA_NAME = MEM and MEM = (SSA_NAME | invariant) patterns.  */
  if (!gimple_assign_single_p (stmt))
    return NULL;

  lhs = gimple_assign_lhs_ptr (stmt);
  rhs = gimple_assign_rhs1_ptr (stmt);

  if (TREE_CODE (*lhs) == SSA_NAME && gimple_vuse (stmt))
    {
      *is_store = false;
      return rhs;
    }
  else if (gimple_vdef (stmt)
	   && (TREE_CODE (*rhs) == SSA_NAME || is_gimple_min_invariant (*rhs)))
    {
      *is_store = true;
      return lhs;
    }
  else
    return NULL;
}

static void
analyse_incremental (gimple *stmt, memref_t* mr)
{
  if (!gimple_assign_single_p (stmt))
    return;
  tree rhs1, rhs2;
  /* TODO: maybe support other types of stmts.  */
  while (stmt && is_gimple_assign (stmt))
    {
      enum tree_code def_code = gimple_assign_rhs_code (stmt);
      gimple_rhs_class rhs_class = gimple_assign_rhs_class (stmt);
      if (dump_file)
	{
	  fprintf (dump_file, "Incr: in assign (%s)\n",
		   get_tree_code_name (def_code));
	  print_gimple_stmt (dump_file, stmt, 3, TDF_DETAILS);
	}
      gcc_assert (def_code != ERROR_MARK);
      switch (rhs_class)
	{
	case GIMPLE_TERNARY_RHS:
	  if (dump_file)
	    fprintf (dump_file, "Incr: unsupported trinary rhs\n");
	  stmt = NULL;
	  break;
	case GIMPLE_UNARY_RHS:
	case GIMPLE_SINGLE_RHS:
	  rhs1 = gimple_assign_rhs1 (stmt);
	  if (dump_file)
	    {
	      fprintf (dump_file, "Incr: (%s)",
		       get_tree_code_name (TREE_CODE (rhs1)));
	      print_generic_expr (dump_file, rhs1);
	      fprintf (dump_file, "\n");
	    }
	  if (def_code == SSA_NAME)
	    stmt = SSA_NAME_DEF_STMT (rhs1);
	  else if (def_code == MEM_REF || def_code == COMPONENT_REF
		   || def_code == ARRAY_REF)
	    {
	      /* If we have dereference in address evaluation,
		 it's indirect memory access.  */
	      if (dump_file)
		{
		  if (operand_equal_p (mr->mem, rhs1))
		    fprintf (dump_file, "Incr: the same MEM\n");
		  else
		    fprintf (dump_file, "Incr: diff MEM\n");
		  print_generic_expr (dump_file, rhs1);
		  fprintf (dump_file, " ");
		  print_generic_expr (dump_file, mr->mem);
		  fprintf (dump_file, "\n");
		}
	      if (operand_equal_p (mr->mem, rhs1) && mr->step)
		mr->is_incr = true;
	      stmt = NULL;
	    }
	  else
	    {
	      if (dump_file)
		fprintf (dump_file, "Incr: unsupported unary/single\n");
	      stmt = NULL;
	    }
	  break;
	case GIMPLE_BINARY_RHS:
	  rhs1 = gimple_assign_rhs1 (stmt);
	  rhs2 = gimple_assign_rhs2 (stmt);
	  if (dump_file)
	    {
	      fprintf (dump_file, "(%s) (%s)",
		       get_tree_code_name (TREE_CODE (rhs1)),
		       get_tree_code_name (TREE_CODE (rhs2)));
	      print_generic_expr (dump_file, rhs1);
	      fprintf (dump_file, " ");
	      print_generic_expr (dump_file, rhs2);
	      fprintf (dump_file, "\n");
	    }
	  /* TODO: extend for other types of incrementation.  */
	  if (TREE_CODE (rhs1) == SSA_NAME && TREE_CODE (rhs2) == INTEGER_CST)
	    {
	      stmt = SSA_NAME_DEF_STMT (rhs1);
	      mr->step = rhs2;
	      if (dump_file)
		{
		  fprintf (dump_file, "Incr: const increment stmt: ");
		  print_gimple_stmt (dump_file, stmt, 3, TDF_DETAILS);
		}
	    }
	  else
	    stmt = NULL;
	  break;
	default:
	  gcc_unreachable ();
      }
    }
  if ((mr->step && !mr->is_incr) || (!mr->step && mr->is_incr))
    {
      mr->step = NULL_TREE;
      mr->is_incr = false;
    }
}

static mr_type
get_memref_type (memref_t *base, memref_t *used, enum tree_code code)
{
  /* TODO: improve memref type detection.  */
  enum tree_code base_code = TREE_CODE (base->mem);
  if (dump_file)
    fprintf (dump_file, "get_memref_type: base=%d,%d used=%d,%d code=%s "
	     "base_code=%s\n", base->mr_id, base->type,
	     used ? used->mr_id : -1, used ? used->type : -1,
	     get_tree_code_name (code), get_tree_code_name (base_code));
  if (used)
    {
      if (base->type > used->type)
	return base->type;
      if (used->type == MR_SIMPLE)
	return MR_POLYNOMIAL;
      if (used->type == MR_POLYNOMIAL)
	return base_code == ARRAY_REF ? MR_POLYNOMIAL : MR_INDIRECT;
      if (used->type == MR_INDIRECT)
	return MR_INDIRECT;
      return MR_UNSUPPORTED;
    }
  if (code == MEM_REF || code == ARRAY_REF || code == COMPONENT_REF)
    return base->type;
  if (code == POINTER_PLUS_EXPR || code == PLUS_EXPR
      || code == MINUS_EXPR || code == MULT_EXPR)
    return base->type <= MR_POLYNOMIAL ? MR_POLYNOMIAL : base->type;
  return base->type >= MR_INDIRECT ? base->type : MR_INDIRECT;
}

/* Recursively walk defs of src expression and record used stmts and other mrs.
   Return a base address candidate if it's found.  */

static tree
analyse_addr_eval (tree src, memref_t* mr)
{
  if (TREE_CODE (src) != SSA_NAME)
    return NULL_TREE;
  gimple *stmt = SSA_NAME_DEF_STMT (src);
  if (dump_file)
    {
      fprintf (dump_file, "Src_stmt: ");
      print_gimple_stmt (dump_file, stmt, 0);
    }
  if (!is_gimple_assign (stmt))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Is not assign, stop analysis: ");
	  print_gimple_stmt (dump_file, stmt, 3, TDF_DETAILS);
	}
      mr->type = MR_UNSUPPORTED;
      mr->stmts.safe_push (stmt);
      return NULL_TREE;
    }
  enum tree_code def_code = gimple_assign_rhs_code (stmt);
  if (def_code != MEM_REF && def_code != COMPONENT_REF
      && def_code != ARRAY_REF)
    mr->stmts.safe_push (stmt);
  gimple_rhs_class rhs_class = gimple_assign_rhs_class (stmt);
  tree rhs1, rhs2, base;
  if (dump_file)
    fprintf (dump_file, "In assign (%s): ", get_tree_code_name (def_code));

  switch (rhs_class)
    {
    case GIMPLE_TERNARY_RHS:
      if (dump_file)
	fprintf (dump_file, "Unsupported trinary rhs\n");
      mr->type = MR_UNSUPPORTED;
      return NULL_TREE;
    case GIMPLE_UNARY_RHS:
    case GIMPLE_SINGLE_RHS:
      rhs1 = gimple_assign_rhs1 (stmt);
      if (dump_file)
	{
    	  fprintf (dump_file, "(%s)",
		   get_tree_code_name (TREE_CODE (rhs1)));
	  print_generic_expr (dump_file, rhs1);
	  fprintf (dump_file, "\n");
	}
      if (def_code == NOP_EXPR)
	return analyse_addr_eval (rhs1, mr);
      else if (def_code == MEM_REF || def_code == COMPONENT_REF
	       || def_code == ARRAY_REF)
	{
	  memref_t *mr2 = get_memref (stmt, rhs1, false);
	  mr->type = get_memref_type (mr, mr2, def_code);
	  for (memref_set::const_iterator it = mr2->used_mrs.begin ();
	       it != mr2->used_mrs.end (); it++)
	    mr->used_mrs.insert (*it);
	  mr->used_mrs.insert (mr2);
	  return mr2->base;
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "Unsupported unary/single\n");
	  mr->type = MR_UNSUPPORTED;
	}
      return NULL_TREE;
    case GIMPLE_BINARY_RHS:
      rhs1 = gimple_assign_rhs1 (stmt);
      rhs2 = gimple_assign_rhs2 (stmt);
      if (dump_file)
	{
    	  fprintf (dump_file, "(%s) (%s)",
		   get_tree_code_name (TREE_CODE (rhs1)),
		   get_tree_code_name (TREE_CODE (rhs2)));
	  print_generic_expr (dump_file, rhs1);
 	  fprintf (dump_file, " ");
	  print_generic_expr (dump_file, rhs2);
	  fprintf (dump_file, "\n");
	}
      base = analyse_addr_eval (rhs1, mr);
      analyse_addr_eval (rhs2, mr);
      mr->type = get_memref_type (mr, NULL, def_code);
      return base;
    default:
      gcc_unreachable ();
    }
  return NULL_TREE;
}

static tree
get_mem_ref_address_ssa_name (tree mem, tree base)
{
  gcc_assert (TREE_CODE (mem) == MEM_REF);
  if (base == NULL_TREE)
    base = get_base_address (mem);
  tree base_addr = NULL_TREE;
  if (TREE_CODE (base) == MEM_REF)
    base_addr = TREE_OPERAND (base, 0);
  if (base_addr != NULL_TREE && TREE_CODE (base_addr) == SSA_NAME)
    return base_addr;
  return NULL_TREE;
}

static void
analyse_mem_ref (gimple *stmt, tree mem, memref_t* mr)
{
  tree base = get_base_address (mem);
  if (dump_file)
    fprintf (dump_file, "Codes: base = %s, mem = %s\n",
	     base ? get_tree_code_name (TREE_CODE (base)) : "null",
	     mem ? get_tree_code_name (TREE_CODE (mem)) : "null");

  mr->stmts.safe_push (stmt);
  mr->base = base;
  switch (TREE_CODE (mem))
    {
    case COMPONENT_REF:
      if (mr->is_store)
	analyse_incremental (stmt, mr);
      mr->type = MR_SIMPLE;
      mr->offset = TREE_OPERAND (mem, 1);
      return;
    case ARRAY_REF:
      analyse_addr_eval (TREE_OPERAND (mem, 1), mr);
      return;
    case MEM_REF:
      {
	tree base_addr = get_mem_ref_address_ssa_name (mem, base);
	if (dump_file)
	  {
	    fprintf (dump_file, "Base addr (%s): ",
		     base_addr ? get_tree_code_name (TREE_CODE (base_addr))
			       : "null");
	    if (base_addr)
	      print_generic_expr (dump_file, base_addr);
	    fprintf (dump_file, "\n");
	  }
	if (base_addr)
	  {
	    mr->base = analyse_addr_eval (base_addr, mr);
	    return;
	  }
	break;
      }
    default:
      break;
    }
  mr->type = MR_UNSUPPORTED;
  mr->base = NULL_TREE;
}

static void
analyse_stmt (gimple *stmt)
{
  bool is_store;
  tree *mem = simple_mem_ref_in_stmt (stmt, &is_store);
  if (!mem)
    return;
  if (dump_file)
    {
      fprintf (dump_file, "\n%s: mr is found in stmt (%s): ",
	       function_name (cfun), is_store ? "store" : "load");
      print_gimple_stmt (dump_file, stmt, 3, TDF_DETAILS);
    }
  memref_t *mr = get_memref (stmt, *mem, is_store);
  (*fmrs_map)[cfun]->insert (mr);
  if (dump_file)
    print_memref (mr);
}

/* Scan stmts for indirect stores/loads with bases passed as function args.  */

static void
collect_memrefs_for_cgraph_node (struct cgraph_node *n)
{
  if (dump_file)
    fprintf (dump_file, "\nCollect indirect ptr info in %s\n", n->dump_name ());
  n->get_body ();
  function *fn = DECL_STRUCT_FUNCTION (n->decl);
  gcc_assert (fn && n->has_gimple_body_p ());

  push_cfun (fn);
  basic_block bb;
  gimple_stmt_iterator si;
  (*fmrs_map)[fn] = new memref_set;
  FOR_EACH_BB_FN (bb, fn)
    for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
      {
	gimple *stmt = gsi_stmt (si);
	analyse_stmt (stmt);
      }
  pop_cfun ();
}

/* Walk cgraph nodes and collect memory references info.  */

static void
collect_memory_references ()
{
  struct cgraph_node *n;
  /* TODO: collect info only for loops and functions in loops.  */
  FOR_EACH_DEFINED_FUNCTION (n)
    if (nl_map->count (n) != 0 && n->has_gimple_body_p ())
      collect_memrefs_for_cgraph_node (n);

  if (dump_file)
    {
      fprintf (dump_file, "\n\nDump mem references:\n");
      FOR_EACH_DEFINED_FUNCTION (n)
	if (nl_map->count (n) != 0 && n->has_gimple_body_p ())
	  {
	    function *fn = DECL_STRUCT_FUNCTION (n->decl);
	    fprintf (dump_file, "\nIn function %s (%s):\n", function_name (fn),
		     nl_map->count (n) != 0 ? "in loop" : "");
	    for (memref_set::const_iterator it = (*fmrs_map)[fn]->begin ();
		 it != (*fmrs_map)[fn]->end (); it++)
	      print_memref (*it);
	  }
    }
}

/* Analysis of loops.  */

memref_set *current_incr_mrs;
memref_set *current_indirect_mrs;

static void
collect_memref (memref_t *mr, class loop *loop, bool check_loop)
{
  gimple *stmt = mr->stmts[0];
  gcc_assert (stmt);
  if (check_loop && !flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
    return;

  /* TODO: Improve base invariant analysis for memrefs which are not local
     (located in called functions).  */
  bool is_base_inv = false;
  if (mr->base)
    is_base_inv = expr_invariant_in_loop_p (loop, mr->base);

  if (dump_file && (mr->type == MR_INDIRECT || mr->is_incr))
    {
      fprintf (dump_file, "%s MR (%d): ", mr->is_incr ? "INCR" : "INDIRECT",
	       mr->mr_id);
      print_generic_expr (dump_file, mr->mem);
      fprintf (dump_file, "\twith base: ");
      if (mr->base)
	print_generic_expr (dump_file, mr->base);
      else
	fprintf (dump_file, "null");
      fprintf (dump_file, " (is_inv=%d)\n", is_base_inv);
    }

  if (!is_base_inv)
    return;
  if (mr->type == MR_INDIRECT)
    current_indirect_mrs->insert (mr);
  if (mr->is_incr)
    current_incr_mrs->insert (mr);
}

static void
analyse_callable_function (struct cgraph_node *n, class loop *loop)
{
  if (dump_file)
    fprintf (dump_file, "Callable (%s):\n", n->dump_name ());

  function *fn = DECL_STRUCT_FUNCTION (n->decl);
  if (fmrs_map->count (fn))
    for (memref_set::const_iterator it = (*fmrs_map)[fn]->begin ();
	 it != (*fmrs_map)[fn]->end (); it++)
      collect_memref (*it, loop, false);
}

static void
insert_node_with_callable_nodes (node_set &s, struct cgraph_node *n)
{
  s.insert (n);
  if (nn_map->count (n) == 0)
    return;
  node_set *set = (*nn_map)[n];
  for (node_set::const_iterator it = set->begin (); it != set->end (); it++)
    s.insert ((*it));
}

static bool
compatible_memrefs_p (memref_t *mr1, memref_t *mr2, bool &compatible_offset)
{
  if (!mr1->base || !mr2->base || !mr2->offset)
    return false;
  tree base_type1 = TYPE_MAIN_VARIANT (TREE_TYPE (mr1->base));
  tree base_type2 = TYPE_MAIN_VARIANT (TREE_TYPE (mr2->base));
  if (base_type1 != base_type2)
    return false;
  if (mr1->offset && mr1->offset == mr2->offset)
    compatible_offset = true;
  else
    compatible_offset = false;
  return true;
}

static void
compare_memrefs (memref_t* mr, memref_t* mr2)
{
  /* TODO: improve analysis of memrefs from different functions: take into
     account data flow and context.  */
  bool compatible_offset = false;
  if (!compatible_memrefs_p (mr, mr2, compatible_offset))
    return;
  if (!compatible_offset)
    {
      for (memref_set::const_iterator it = mr->used_mrs.begin ();
	   it != mr->used_mrs.end (); it++)
	if ((*it)->offset && (*it)->offset == mr2->offset)
	  {
	    compatible_offset = true;
	    if (dump_file)
	      fprintf (dump_file, "Used MR (%d) and INC MR have "
		       "the same offset\n", (*it)->mr_id);
	    break;
	  }
    }
  if (!compatible_offset)
    return;
  if (dump_file)
    {
      fprintf (dump_file, "MR (%d) is optimization candidate with offset: ",
	       mr->mr_id);
      print_generic_expr (dump_file, mr2->offset);
      fprintf (dump_file, "\n");
    }

  if (!mr_candidate_map->count (mr))
    {
      (*mr_candidate_map)[mr] = mr2;
      return;
    }
  /* TODO: support analysis with incrementation of different fields.  */
  if ((*mr_candidate_map)[mr]->offset != mr2->offset)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "It conflicts with previously found MR (%d) "
		   "with offset ", (*mr_candidate_map)[mr]->mr_id);
	  if ((*mr_candidate_map)[mr] != NULL)
	    print_generic_expr (dump_file, (*mr_candidate_map)[mr]->offset);
	  fprintf (dump_file, ", disable the optimization\n");
	}
      (*mr_candidate_map)[mr] = NULL;
    }
}

/* In the given loop and all functions called from the loop, collect
   indirect/incremental memrefs with invariant base address and inductive
   offset.  */

static void
collect_memrefs_for_loop (class loop *loop, struct cgraph_node *n,
			  function *fn)
{
  current_incr_mrs = new memref_set;
  current_indirect_mrs = new memref_set;

  if (dump_file)
    fprintf (dump_file, "Loop %d\n", loop->num);
  if (fmrs_map->count (fn))
    for (memref_set::const_iterator it = (*fmrs_map)[fn]->begin ();
	 it != (*fmrs_map)[fn]->end (); it++)
      collect_memref (*it, loop, true);

  /* Collect vector of functions called in the loop.  */
  node_set set;
  struct cgraph_edge *e;
  struct cgraph_node *n2;
  for (e = n->callees; e; e = e->next_callee)
    {
      gcall *stmt = e->call_stmt;
      if (!flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
	continue;
      insert_node_with_callable_nodes (set, e->callee);
    }
  for (e = n->indirect_calls; e; e = e->next_callee)
    {
      gcall *stmt = e->call_stmt;
      if (!flow_bb_inside_loop_p (loop, gimple_bb (stmt))
	  || !e->indirect_info->targets)
	continue;
      for (unsigned i = 0; e->indirect_info->targets->iterate (i, &n2); ++i)
	insert_node_with_callable_nodes (set, n2);
    }
  if (set.empty ())
    return;
  if (dump_file)
    fprintf (dump_file, "Go inside all callables of %s\n", n->dump_name ());

  for (node_set::const_iterator it = set.begin (); it != set.end (); it++)
    analyse_callable_function (*it, loop);

  if (!current_incr_mrs->empty () && !current_indirect_mrs->empty ())
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Loop has both incr and indirect memrefs\n"
		   "Incr: ");
	  for (memref_set::const_iterator it = current_incr_mrs->begin ();
	       it != current_incr_mrs->end (); it++)
	    fprintf (dump_file, "%d ", (*it)->mr_id);
	  fprintf (dump_file, "\nIndirect: ");
	  for (memref_set::const_iterator it = current_indirect_mrs->begin ();
	       it != current_indirect_mrs->end (); it++)
	    fprintf (dump_file, "%d ", (*it)->mr_id);
	  fprintf (dump_file, "\n");
	}
      /* Check if indirect memref has a base address similar to one of
	 incremental memref.  */
      for (memref_set::const_iterator it = current_indirect_mrs->begin ();
	   it != current_indirect_mrs->end (); it++)
	for (memref_set::const_iterator it2 = current_incr_mrs->begin ();
	   it2 != current_incr_mrs->end (); it2++)
	  compare_memrefs (*it, *it2);
    }

  delete current_incr_mrs;
  delete current_indirect_mrs;
}

static void
analyse_loops_in_cgraph_node (struct cgraph_node *n)
{
  if (dump_file)
    fprintf (dump_file, "\nAnalyse loops in %s\n", n->dump_name ());

  n->get_body ();
  function *fn = DECL_STRUCT_FUNCTION (n->decl);
  gcc_assert (fn && n->has_gimple_body_p ());

  push_cfun (fn);
  calculate_dominance_info (CDI_DOMINATORS);
  loop_optimizer_init (LOOPS_NORMAL);

  class loop *loop;
  FOR_EACH_LOOP (loop, 0)
    {
      class loop *outer = loop_outer (loop);
      /* Walk only outermost loops.  */
      if (outer->num != 0)
	continue;
      collect_memrefs_for_loop (loop, n, fn);
    }

  free_dominance_info (CDI_DOMINATORS);
  loop_optimizer_finalize ();
  pop_cfun ();
}

static void
analyse_loops ()
{
  if (dump_file)
    fprintf (dump_file, "\n\nLoops: procesing functions\n");
  cgraph_node *n;
  FOR_EACH_DEFINED_FUNCTION (n)
    {
      if (!can_be_optimized (n))
	{
	  if (dump_file)
	    fprintf (dump_file, "Skip the function\n");
	  continue;
	}
      analyse_loops_in_cgraph_node (n);
    }

  if (dump_file)
    fprintf (dump_file, "\n\nList of optimization candidates:\n");

  FOR_EACH_DEFINED_FUNCTION (n)
    {
      function *fn = DECL_STRUCT_FUNCTION (n->decl);
      if (!can_be_optimized (n) || !fmrs_map->count (fn))
	continue;
      for (memref_map::iterator it = mr_candidate_map->begin ();
	   it != mr_candidate_map->end (); ++it)
	{
	  memref_t *mr = it->first, *mr2 = it->second;
	  if (mr2 == NULL || !(*fmrs_map)[fn]->count (mr))
	    continue;
	  /* For now optimize only MRs that mem is MEM_REF.
	     TODO: support other MR types.  */
	  if (TREE_CODE (mr->mem) != MEM_REF)
	    {
	      if (dump_file)
		fprintf (dump_file, "Skip MR %d: unsupported tree code = %s\n",
			 mr->mr_id, get_tree_code_name (TREE_CODE (mr->mem)));
	      continue;
	    }
	  if (!optimize_mrs_map->count (fn))
	    (*optimize_mrs_map)[fn] = new memref_set;
	  (*optimize_mrs_map)[fn]->insert (mr);
	}
      if (dump_file && optimize_mrs_map->count (fn))
	{
	  fprintf (dump_file, "Function %s\n", n->dump_name ());
	  for (memref_set::const_iterator it
		   = (*optimize_mrs_map)[fn]->begin ();
	       it != (*optimize_mrs_map)[fn]->end (); it++)
	    {
	      memref_t *mr = *it, *mr2 = (*mr_candidate_map)[mr];
	      fprintf (dump_file, "MRs %d, %d with incremental offset ",
		       mr->mr_id, mr2->mr_id);
	      print_generic_expr (dump_file, mr2->offset);
	      fprintf (dump_file, "\n");
	    }
	}
    }
}

/* Reduce the set filtering out memrefs with the same memory references,
   return the result vector of memrefs.  */

static void
reduce_memref_set (memref_set *set, vec<memref_t *> &vec)
{
  for (memref_set::const_iterator it = set->begin ();
      it != set->end (); it++)
    {
      memref_t *mr1 = *it;
      if (!vec.length ())
	vec.safe_push (mr1);
      else
	{
	  bool inserted = false;
	  for (unsigned int i = 0; i < vec.length (); i++)
	    {
	      /* mr2 is less than current mr1.  */
	      memref_t *mr2 = vec[i];
	      if (operand_equal_p (mr1->mem, mr2->mem))
		{
		  if (dump_file)
		    fprintf (dump_file, "The same mems in MRs %d and %d\n",
			     mr1->mr_id, mr2->mr_id);
		  /* TODO: maybe build new memref which include stmts of both
		     mr1 and mr2.  */
		  if ((mr1->is_store && !mr2->is_store)
		       || mr1->stmts.length () > mr2->stmts.length ())
		    {
		      inserted = true;
		      vec[i] = mr1;
		    }
		}
	    }
	  if (!inserted)
	    vec.safe_push (mr1);
	}
    }
  if (dump_file)
    {
      fprintf (dump_file, "MRs (%d) after filtering: ", vec.length ());
      for (unsigned int i = 0; i < vec.length (); i++)
	fprintf (dump_file, "%d ", vec[i]->mr_id);
      fprintf (dump_file, "\n");
    }
}

static void
find_nearest_common_dominator (memref_t *mr, basic_block &dom)
{
  for (unsigned int i = 0; i < mr->stmts.length (); i++)
    {
      basic_block bb = gimple_bb (mr->stmts[i]);
      gcc_assert (bb);
      if (dom == bb)
	continue;
      if (dom)
	dom = nearest_common_dominator (CDI_DOMINATORS, dom, bb);
      else
	dom = bb;
    }
}

/* Return true if DECL is a parameter or a SSA_NAME for a parameter.
   TODO: from gcc/tree-inline.c, maybe make it global.  */

static bool
is_parm (tree decl)
{
  if (TREE_CODE (decl) == SSA_NAME)
    {
      decl = SSA_NAME_VAR (decl);
      if (!decl)
	return false;
    }

  return (TREE_CODE (decl) == PARM_DECL);
}

/* TODO: the following functions are inspired by remap in gcc/tree-inline.c,
   maybe we can share some functionality.  */

static tree
remap_name (tree name, gimple *stmt, bool is_lhs)
{
  tree new_tree = NULL_TREE;
  if (decl_map->count (name))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Find map: ");
	  print_generic_expr (dump_file, name);
	  fprintf (dump_file, " ");
	  print_generic_expr (dump_file, (*decl_map)[name]);
	  fprintf (dump_file, "\n");
	}
      return unshare_expr ((*decl_map)[name]);
    }
  if (!is_lhs)
    return name;
  if (TREE_CODE (name) == SSA_NAME)
    {
      /* Remap anonymous SSA names or SSA names of anonymous decls.  */
      tree var = SSA_NAME_VAR (name);
      if (!var
	  || (!SSA_NAME_IS_DEFAULT_DEF (name)
	      && VAR_P (var) && !VAR_DECL_IS_VIRTUAL_OPERAND (var)
	      && DECL_ARTIFICIAL (var) && DECL_IGNORED_P (var)
	      && !DECL_NAME (var)))
	{
	  new_tree = make_ssa_name (TREE_TYPE (name), stmt);
	  if (!var && SSA_NAME_IDENTIFIER (name))
	    SET_SSA_NAME_VAR_OR_IDENTIFIER (new_tree,
					    SSA_NAME_IDENTIFIER (name));
	  SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_tree)
	      = SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name);
	  /* So can range-info.  */
	  if (!POINTER_TYPE_P (TREE_TYPE (name))
	      && SSA_NAME_RANGE_INFO (name))
	    duplicate_ssa_name_range_info (new_tree,
					   SSA_NAME_RANGE_TYPE (name),
					   SSA_NAME_RANGE_INFO (name));
	  /* TODO: maybe correct the insertion.  */
	  (*decl_map)[name] = new_tree;
	  if (dump_file)
	    {
	      fprintf (dump_file, "New map (no var): ");
	      print_generic_expr (dump_file, name);
	      fprintf (dump_file, " ");
	      print_generic_expr (dump_file, new_tree);
	      fprintf (dump_file, "\n");
	    }
	  return new_tree;
	}
      /* TODO: maybe remap_name or do the same as before for SSA_NAME_VAR.  */
      new_tree = make_ssa_name (TREE_TYPE (name), stmt);
      (*decl_map)[name] = new_tree;
      if (dump_file)
	{
	  fprintf (dump_file, "New map: ");
	  print_generic_expr (dump_file, name);
	  fprintf (dump_file, " ");
	  print_generic_expr (dump_file, new_tree);
	  fprintf (dump_file, "\n");
	}
    }
  else if (VAR_P (name) || TREE_CODE (name) == PARM_DECL)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "VAR/PARM: ");
	  print_generic_expr (dump_file, name);
	  fprintf (dump_file, "\n");
	}
      return name;
    }
  else
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Unsupported: ");
	  print_generic_expr (dump_file, name);
	  fprintf (dump_file, "\n");
	}
      //gcc_unreachable ();
      return name;
    }
  return new_tree;
}

/* Passed to walk_tree.  Copies the node pointed to, if appropriate.  */

static tree
ipa_copy_tree_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*tp);
  enum tree_code_class cl = TREE_CODE_CLASS (code);

  /* We make copies of most nodes.  */
  if (IS_EXPR_CODE_CLASS (cl)
      || code == TREE_LIST
      || code == TREE_VEC
      || code == TYPE_DECL
      || code == OMP_CLAUSE)
    {
      /* Because the chain gets clobbered when we make a copy, we save it
	 here.  */
      tree chain = NULL_TREE, new_tree;

      if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
	chain = TREE_CHAIN (*tp);

      /* Copy the node.  */
      new_tree = copy_node (*tp);

      *tp = new_tree;

      /* Now, restore the chain, if appropriate.  That will cause
	 walk_tree to walk into the chain as well.  */
      if (code == PARM_DECL
	  || code == TREE_LIST
	  || code == OMP_CLAUSE)
	TREE_CHAIN (*tp) = chain;

      /* For now, we don't update BLOCKs when we make copies.  So, we
	 have to nullify all BIND_EXPRs.  */
      if (TREE_CODE (*tp) == BIND_EXPR)
	BIND_EXPR_BLOCK (*tp) = NULL_TREE;
    }
  else if (code == CONSTRUCTOR || code == STATEMENT_LIST)
    gcc_unreachable ();
  else if (TREE_CODE_CLASS (code) == tcc_type
	   || TREE_CODE_CLASS (code) == tcc_declaration
	   || TREE_CODE_CLASS (code) == tcc_constant)
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Remap the GIMPLE operand pointed to by *TP.  DATA is really a
   'struct walk_stmt_info *'.  DATA->INFO is a 'gimple *'.
   WALK_SUBTREES is used to indicate walk_gimple_op whether to keep
   recursing into the children nodes of *TP.  */

static tree
remap_gimple_op_r (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi_p = (struct walk_stmt_info *) data;
  gimple *stmt = (gimple *) wi_p->info;

  /* For recursive invocations this is no longer the LHS itself.  */
  bool is_lhs = wi_p->is_lhs;
  wi_p->is_lhs = false;

  if (TREE_CODE (*tp) == SSA_NAME)
    {
      *tp = remap_name (*tp, stmt, is_lhs);
      *walk_subtrees = 0;
      if (is_lhs)
	SSA_NAME_DEF_STMT (*tp) = wi_p->stmt;
      return NULL;
    }
  else if (auto_var_in_fn_p (*tp, cfun->decl))
    {
      /* Local variables and labels need to be replaced by equivalent
	 variables.  We don't want to copy static variables; there's
	 only one of those, no matter how many times we inline the
	 containing function.  Similarly for globals from an outer
	 function.  */
      tree new_decl;

      /* Remap the declaration.  */
      new_decl = remap_name (*tp, stmt, is_lhs);
      gcc_assert (new_decl);
      /* Replace this variable with the copy.  */
      STRIP_TYPE_NOPS (new_decl);
      /* ???  The C++ frontend uses void * pointer zero to initialize
	 any other type.  This confuses the middle-end type verification.
	 As cloned bodies do not go through gimplification again the fixup
	 there doesn't trigger.  */
      if (TREE_CODE (new_decl) == INTEGER_CST
	  && !useless_type_conversion_p (TREE_TYPE (*tp), TREE_TYPE (new_decl)))
	new_decl = fold_convert (TREE_TYPE (*tp), new_decl);
      *tp = new_decl;
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (*tp) == STATEMENT_LIST || TREE_CODE (*tp) == SAVE_EXPR)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Unexpected tree: ");
	  print_generic_expr (dump_file, *tp);
	  fprintf (dump_file, "\n");
	}
      gcc_unreachable ();
    }
  else
    {
      /* Otherwise, just copy the node.  Note that copy_tree_r already
	 knows not to copy VAR_DECLs, etc., so this is safe.  */

      if (TREE_CODE (*tp) == MEM_REF)
	{
	  /* We need to re-canonicalize MEM_REFs from inline substitutions
	     that can happen when a pointer argument is an ADDR_EXPR.
	     Recurse here manually to allow that.  */
	  tree ptr = TREE_OPERAND (*tp, 0);
	  tree type = TREE_TYPE (*tp);
	  tree old = *tp;
	  walk_tree (&ptr, remap_gimple_op_r, data, NULL);
	  *tp = fold_build2 (MEM_REF, type, ptr, TREE_OPERAND (*tp, 1));
	  TREE_THIS_VOLATILE (*tp) = TREE_THIS_VOLATILE (old);
	  TREE_SIDE_EFFECTS (*tp) = TREE_SIDE_EFFECTS (old);
	  TREE_NO_WARNING (*tp) = TREE_NO_WARNING (old);
	  /* TODO: maybe support this case.  */
	  gcc_assert (MR_DEPENDENCE_CLIQUE (old) == 0);
	  /* We cannot propagate the TREE_THIS_NOTRAP flag if we have
	     remapped a parameter as the property might be valid only
	     for the parameter itself.  */
	  if (TREE_THIS_NOTRAP (old) && (!is_parm (TREE_OPERAND (old, 0))))
	    TREE_THIS_NOTRAP (*tp) = 1;
	  REF_REVERSE_STORAGE_ORDER (*tp) = REF_REVERSE_STORAGE_ORDER (old);
	  *walk_subtrees = 0;
	  return NULL;
	}

      /* Here is the "usual case".  Copy this tree node, and then
	 tweak some special cases.  */
      ipa_copy_tree_r (tp, walk_subtrees, NULL);
      gcc_assert (!(TREE_CODE (*tp) == TARGET_EXPR && TREE_OPERAND (*tp, 3)));
      if (TREE_CODE (*tp) == ADDR_EXPR)
	{
	  /* TODO: If this used to be invariant, but is not any longer,
	     then regimplification is probably needed.  */
	  walk_tree (&TREE_OPERAND (*tp, 0), remap_gimple_op_r, data, NULL);
	  recompute_tree_invariant_for_addr_expr (*tp);
	  *walk_subtrees = 0;
	}
    }
  /* TODO: maybe we need to update TREE_BLOCK (*tp).  */

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Copy stmt and remap its operands.  */

static gimple *
gimple_copy_and_remap (gimple *stmt)
{
  gimple *copy = gimple_copy (stmt);
  gcc_checking_assert (!is_gimple_debug (copy));

  /* Remap all the operands in COPY.  */
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  wi.info = copy;
  walk_gimple_op (copy, remap_gimple_op_r, &wi);
  if (dump_file)
    {
      fprintf (dump_file, "Stmt copy after remap:\n");
      print_gimple_stmt (dump_file, copy, 0);
    }
  return copy;
}

static void
create_cgraph_edge (cgraph_node *n, gimple *stmt)
{
  gcall *call_stmt = dyn_cast <gcall *> (stmt);
  basic_block bb = gimple_bb (stmt);
  tree decl = gimple_call_fndecl (call_stmt);
  if (!decl)
    return;
  struct cgraph_edge *e = n->create_edge (cgraph_node::get_create (decl),
					  call_stmt, bb->count);
  /* TODO: maybe we need to store ipa_call_summary result.  */
  ipa_call_summaries->get_create (e);
}

/* Insert prefetch intrinsics in this function, return nonzero on success.  */

static int
optimize_function (cgraph_node *n, function *fn)
{
  /* In a given function, optimize only indirect memrefs with
     the same incremental memref.
     TODO: implement the optimization for other cases.  */
  bool different_incrementals = false;
  memref_t *first_mr = NULL;
  memref_set used_mrs;
  for (memref_set::const_iterator it = (*optimize_mrs_map)[fn]->begin ();
       it != (*optimize_mrs_map)[fn]->end (); it++)
    {
      memref_t *mr = *it;
      if (!first_mr)
	first_mr = mr;
      else if ((*mr_candidate_map)[first_mr] != (*mr_candidate_map)[mr])
	{
	  different_incrementals = true;
	  break;
	}
      for (memref_set::const_iterator it2 = mr->used_mrs.begin ();
	   it2 != mr->used_mrs.end (); it2++)
	used_mrs.insert (*it2);
    }
  if (different_incrementals)
    {
      if (dump_file)
	fprintf (dump_file, "It contains memrefs with different "
		 "incrementals.  Skip the case.\n");
      return 0;
    }
  memref_t *inc_mr = (*mr_candidate_map)[first_mr];
  if (!inc_mr->stmts[0] || !gimple_assign_single_p (inc_mr->stmts[0]))
    {
      if (dump_file)
	fprintf (dump_file, "Incremental MR with unexpected stmt.  "
		 "Skip the case.\n");
      return 0;
    }
  if (!tree_fits_shwi_p (inc_mr->step))
    {
      if (dump_file)
	fprintf (dump_file, "Cannot represent incremental MR's step as "
		 "integer.  Skip the case.\n");
      return 0;
    }
  if (dump_file && !used_mrs.empty ())
    print_mrs_ids (used_mrs, "Common list of used mrs:\n");

  /* Find a memref in used mrs which corresponds to the found incremental
     memref.  */
  memref_t *comp_mr = NULL;
  for (memref_set::const_iterator it = used_mrs.begin ();
       it != used_mrs.end (); it++)
    {
    bool c_offset;
    if ((*it)->type != MR_SIMPLE || inc_mr->type != MR_SIMPLE
	|| !compatible_memrefs_p (*it, inc_mr, c_offset))
      continue;
    if (c_offset)
      {
	if (dump_file)
	  fprintf (dump_file, "Found compatible used MR (%d) and "
		   "incr MR (%d)\n", (*it)->mr_id, inc_mr->mr_id);
	comp_mr = (*it);
      }
    }
  if (!comp_mr || !comp_mr->stmts[0]
      || !gimple_assign_single_p (comp_mr->stmts[0]))
    {
      if (dump_file)
	fprintf (dump_file, "Compatible MR in this function is not found "
		 " or it has unexpected stmt.  Skip the case.\n");
      return 0;
    }

  /* Filter out memrefs with the same memory references.
     TODO: maybe do the same with used mrs.  */
  vec<memref_t *> vmrs = vNULL;
  reduce_memref_set ((*optimize_mrs_map)[fn], vmrs);

  /* Find insertion place.  Create new BB.  */
  /* TODO: maybe it is useful to process also used_mrs.  */
  basic_block dom_bb = NULL;
  for (unsigned int i = 0; i < vmrs.length (); i++)
    find_nearest_common_dominator (vmrs[i], dom_bb);

  if (!dom_bb)
    {
      if (dump_file)
	fprintf (dump_file, "Dominator bb for MRs is not found.  "
		 "Skip the case.\n");
      return 0;
    }
  else if (dump_file)
    {
      fprintf (dump_file, "Dominator bb %d for MRs:\n", dom_bb->index);
      gimple_dump_bb (dump_file, dom_bb, 0, dump_flags);
      fprintf (dump_file, "\n");
    }

  /* Try to find comp_mr's stmt in the dominator bb.  */
  gimple *last_used = NULL;
  for (gimple_stmt_iterator si = gsi_last_bb (dom_bb); !gsi_end_p (si);
       gsi_prev (&si))
    if (comp_mr->stmts[0] == gsi_stmt (si))
      {
	last_used = gsi_stmt (si);
	if (dump_file)
	  {
	    fprintf (dump_file, "Last used stmt in dominator bb:\n");
	    print_gimple_stmt (dump_file, last_used, 0);
	  }
	break;
      }

  split_block (dom_bb, last_used);
  gimple_stmt_iterator gsi = gsi_last_bb (dom_bb);

  /* Create new inc var.  Insert new_var = old_var + step * factor.  */
  decl_map = new tree_map;
  gcc_assert (comp_mr->stmts[0] && gimple_assign_single_p (comp_mr->stmts[0]));
  tree inc_var = gimple_assign_lhs (comp_mr->stmts[0]);
  gimple_seq stmts = NULL;
  tree var_type = TREE_TYPE (inc_var);
  enum tree_code inc_code;
  if (TREE_CODE (var_type) == POINTER_TYPE)
    inc_code = POINTER_PLUS_EXPR;
  else
    inc_code = PLUS_EXPR;
  tree step = inc_mr->step;
  HOST_WIDE_INT dist_val = tree_to_shwi (step)
			   * param_ipa_prefetch_distance_factor;
  tree dist = build_int_cst (TREE_TYPE (step), dist_val);
  tree new_inc_var = gimple_build (&stmts, inc_code, var_type, inc_var, dist);
  (*decl_map)[inc_var] = new_inc_var;
  if (dump_file)
    {
      fprintf (dump_file, "New distance value: %ld, new inc var: ", dist_val);
      print_generic_expr (dump_file, new_inc_var);
      fprintf (dump_file, "\n");
    }

  /* Create other new vars.  Insert new stmts.  */
  stmt_set processed_stmts;
  for (memref_set::const_iterator it = used_mrs.begin ();
       it != used_mrs.end (); it++)
    {
      memref_t *mr = *it;
      gimple *last_stmt = NULL;
      if (mr == comp_mr)
	continue;
      for (int i = mr->stmts.length () - 1; i >= 0 ; i--)
	{
	  if (processed_stmts.count (mr->stmts[i]))
	    continue;
	  processed_stmts.insert (mr->stmts[i]);
	  if (dump_file)
	    {
	      fprintf (dump_file, "Copy stmt %d from used MR (%d):\n",
		       i, mr->mr_id);
	      print_gimple_stmt (dump_file, mr->stmts[i], 0);
	    }
	  /* Create a new copy of STMT and duplicate STMT's virtual
	     operands.  */
	  last_stmt = gimple_copy_and_remap (mr->stmts[i]);
	  gimple_seq_add_stmt (&stmts, last_stmt);
	}
      gcc_assert (last_stmt);
      if (dump_file)
	{
	  fprintf (dump_file, "MR (%d) new mem: ", mr->mr_id);
	  print_generic_expr (dump_file, gimple_assign_lhs (last_stmt));
	  fprintf (dump_file, "\n");
	}
    }
  /* On new load check page fault.  */
  /* Insert prefetch instructions.  */
  if (dump_file)
    fprintf (dump_file, "Evaluate addresses and insert prefetch insn.\n");

  vec<gimple *> pcalls = vNULL;
  tree local;
  switch (param_ipa_prefetch_locality)
    {
    case 0:
      local = integer_zero_node;
      break;
    case 1:
      local = integer_one_node;
      break;
    case 2:
      local = build_int_cst (integer_type_node, 2);
      break;
    default:
    case 3:
      local = integer_three_node;
      break;
    }
  for (unsigned int j = 0; j < vmrs.length (); j++)
    {
      memref_t *mr = vmrs[j];
      /* Don't need to copy the last stmt, since we insert prefetch insn
	 instead of it.  */
      for (int i = mr->stmts.length () - 1; i >= 1 ; i--)
	{
	  if (processed_stmts.count (mr->stmts[i]))
	    continue;
	  processed_stmts.insert (mr->stmts[i]);

	  gimple *copy = gimple_copy_and_remap (mr->stmts[i]);
	  gimple_seq_add_stmt (&stmts, copy);
	}
      gimple *last_stmt = mr->stmts[0];
      gcc_assert (last_stmt);
      tree write_p = mr->is_store ? integer_one_node : integer_zero_node;
      tree addr = get_mem_ref_address_ssa_name (mr->mem, NULL_TREE);
      if (decl_map->count (addr))
	addr = (*decl_map)[addr];
      last_stmt = gimple_build_call (builtin_decl_explicit (BUILT_IN_PREFETCH),
				     3, addr, write_p, local);
      pcalls.safe_push (last_stmt);
      gimple_seq_add_stmt (&stmts, last_stmt);
      if (dump_file)
	{
	  fprintf (dump_file, "Insert %d prefetch stmt:\n", j);
	  print_gimple_stmt (dump_file, last_stmt, 0);
	}
    }

  gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  delete decl_map;

  /* Modify cgraph inserting calls to prefetch intrinsics.  */
  for (unsigned i = 0; i < pcalls.length (); i++)
    create_cgraph_edge (n, pcalls[i]);
  ipa_update_overall_fn_summary (n);

  return 1;
}

static int
insert_prefetch ()
{
  int res = 0;
  cgraph_node *n;
  FOR_EACH_DEFINED_FUNCTION (n)
    {
      function *fn = DECL_STRUCT_FUNCTION (n->decl);
      if (!optimize_mrs_map->count (fn))
	continue;
      if (dump_file)
	fprintf (dump_file, "Optimize function %s\n", n->dump_name ());
      push_cfun (DECL_STRUCT_FUNCTION (n->decl));
      calculate_dominance_info (CDI_DOMINATORS);
      res |= optimize_function (n, fn);
      free_dominance_info (CDI_DOMINATORS);
      pop_cfun ();
    }
  return res;
}

static unsigned int
ipa_prefetch (void)
{
  if (!targetm.have_prefetch ())
    {
      if (dump_file)
	fprintf (dump_file, "Prefetch is not supported by the target.\n");
      return 0;
    }

  unsigned int ret = 0;
  el_map = new edge_in_loop;
  nl_map = new node_in_loop;
  icn_map = new node_to_iedge_map;
  nn_map = new node_to_node_map;
  tm_map = new tree_memref_map;
  fmrs_map = new funct_mrs_map;
  mr_candidate_map = new memref_map;
  optimize_mrs_map = new funct_mrs_map;

  max_mr_id = 0;
  /* TODO: check if we really need this init.  */
  if (!builtin_decl_explicit_p (BUILT_IN_PREFETCH))
    {
      tree type = build_function_type_list (void_type_node,
					    const_ptr_type_node, NULL_TREE);
      tree decl = add_builtin_function ("__builtin_prefetch", type,
					BUILT_IN_PREFETCH, BUILT_IN_NORMAL,
					NULL, NULL_TREE);
      DECL_IS_NOVOPS (decl) = true;
      set_builtin_decl (BUILT_IN_PREFETCH, decl, false);
    }

  analyse_cgraph ();
  prepare_indirect_call_info ();
  propagate_loop_info_in_cgraph ();
  collect_memory_references ();
  analyse_loops ();

  /* TODO: implement some specific heuristics.  */
  if (!optimize_mrs_map->empty ())
    ret = insert_prefetch ();

  delete el_map;
  delete nl_map;
  for (node_to_iedge_map::iterator it = icn_map->begin ();
       it != icn_map->end (); ++it)
    delete it->second;
  delete icn_map;
  for (node_to_node_map::iterator it = nn_map->begin ();
       it != nn_map->end (); ++it)
    delete it->second;
  delete nn_map;
  for (tree_memref_map::iterator it = tm_map->begin ();
       it != tm_map->end (); ++it)
    delete it->second;
  delete tm_map;
  for (funct_mrs_map::iterator it = fmrs_map->begin ();
       it != fmrs_map->end (); ++it)
    delete it->second;
  delete fmrs_map;
  delete mr_candidate_map;
  delete optimize_mrs_map;

  /* TODO: maybe add other todos.  */
  return ret | TODO_verify_all;
}

const pass_data pass_data_ipa_prefetch =
{
  SIMPLE_IPA_PASS, // type
  "ipa_prefetch", // name
  OPTGROUP_NONE, // optinfo_flags
  TV_IPA_PREFETCH, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  0, // todo_flags_finish
};

class pass_ipa_prefetch : public simple_ipa_opt_pass
{
public:
  pass_ipa_prefetch (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_prefetch, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *)
  {
    return ipa_prefetch ();
  }
}; // class pass_ipa_prefetch

bool
pass_ipa_prefetch::gate (function *)
{
  return (optimize >= 3
	  && flag_ipa_prefetch
	  /* Don't bother doing anything if the program has errors.  */
	  && !seen_error ()
	  && flag_lto_partition == LTO_PARTITION_ONE
	  /* Only enable prefetch optimizations in lto or whole_program.  */
	  && (in_lto_p || flag_whole_program));
}

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_prefetch (gcc::context *ctxt)
{
  return new pass_ipa_prefetch (ctxt);
}
