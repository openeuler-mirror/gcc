/* LLC allocate.
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
#define INCLUDE_MAP
#define INCLUDE_SET
#define INCLUDE_VECTOR
#define INCLUDE_LIST
#define INCLUDE_ALGORITHM
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "optabs-query.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "ssa.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "langhooks.h"
#include "tree-inline.h"
#include "tree-data-ref.h"
#include "diagnostic-core.h"
#include "dbgcnt.h"
#include "gimple-pretty-print.h"
#include "internal-fn.h"
#include "tree-cfg.h"
#include "profile-count.h"

/* Number of parallel cores.  */
const unsigned int PARALLEL_NUM = 288;

/* Indirect access weight.  */
const unsigned int INDIRECT_ACCESS_VALUE = 2;

/* Write memory weight.  */
const unsigned int WRITE_COST = 2;

/* Prefetch tool input max length.  */
#ifndef PREFETCH_TOOL_INPUT_MAX_LEN
#define PREFETCH_TOOL_INPUT_MAX_LEN 512
#endif

/* Prefetch tool number max length.  */
#ifndef PREFETCH_TOOL_NUM_MAX_LEN
#define PREFETCH_TOOL_NUM_MAX_LEN 9
#endif

namespace {

using namespace std;

/* loop bound info of the memory reference located.  */
struct loop_bound
{
  /* iv tree_node.  */
  tree iv;

  /* define stmt of iv.  */
  gimple *def_stmt;

  /* loop where stmt is located.  */
  class loop *loop;

  /* loop unroll factor.  */
  unsigned int unroll;

  /* Number of iterations of loop.  */
  tree niters;

  loop_bound (tree t, gimple *stmt)
    {
      iv = t;
      def_stmt = stmt;
      loop = loop_containing_stmt (stmt);
      unroll = 1;
      niters = chrec_dont_know;
    }
};

/* method of calculating the data size.  */

enum calc_type
{
  UNHANDLE_CALC = 0,
  RUNTIME_CALC,
  STATIC_CALC
};

/* Describes a info of a memory reference.  */

struct data_ref
{
  /* The memory reference.  */
  tree ref;

  /* Statement where the ref is located.  */
  gimple *stmt;

  /* var_decl or param_decl, used for the ref_group.  */
  tree var;

  /* Base of the reference.  */
  tree base;

  /* Constant offset of the reference.  */
  tree offset;

  /* index of the reference.  */
  tree index;

  /* Constant step of the reference.  */
  tree step;

  /* loop boundary info of each dimension.  */
  vector<loop_bound> loop_bounds;

  /* memory data size, Unit: MB.  */
  double data_size;

  /* method of calculating the data size.  */
  calc_type calc_by;

  /* True if the info of ref is traced, and then record it.  */
  unsigned int trace_status_p : 1;

  /* True if the loop is vectorized.  */
  unsigned int vectorize_p : 1;

  /* True if the memory reference is shared.  */
  unsigned int parallel_p : 1;

  /* True if the memory reference is regular.  */
  unsigned int regular_p : 1;

  /* True if the memory reference is read.  */
  unsigned int read_p : 1;

  data_ref ()
    {
      ref = NULL_TREE;
      stmt = NULL;
      var = NULL_TREE;
      base = NULL_TREE;
      offset = NULL_TREE;
      index = NULL_TREE;
      step = NULL_TREE;
      data_size = 0;
      calc_by = UNHANDLE_CALC;
      trace_status_p = false;
      vectorize_p = false;
      parallel_p = false;
      regular_p = true;
      read_p = true;
    }
};

/* ================ phase 1 get_dense_memory_kernels ================  */

/* Add ref node and print.  */

void
add_ref (vector<data_ref> &references, tree op, gimple *stmt,
	 bool vectorize_p, bool read_p)
{
  data_ref ref;
  ref.ref = op;
  ref.stmt = stmt;
  ref.vectorize_p = vectorize_p;
  ref.read_p = read_p;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_generic_expr (dump_file, ref.ref, TDF_LINENO);
      fprintf (dump_file, "\n");
    }
  references.push_back (ref);
}

/* Get the references from the simple call (vectorization type).  */

void
get_references_in_gimple_call (gimple *stmt, vector<data_ref> &references)
{
  if (gimple_code (stmt) != GIMPLE_CALL)
    return;

  if (gimple_call_internal_p (stmt))
    {
      bool read_p = false;
      switch (gimple_call_internal_fn (stmt))
	{
	  case IFN_MASK_GATHER_LOAD:
	  case IFN_MASK_LOAD:
	    {
	      if (gimple_call_lhs (stmt) == NULL_TREE)
		return;
	      read_p = true;
	      // FALLTHRU
	    }
	  case IFN_MASK_STORE:
	    {
	      /* _1 = &MEM[base: a_2(D), index: ivtmp_3, step: 8, offset: 0B];
		 vect__1.1 = .MASK_LOAD (_1, 64B, loop_mask_4);

		 _1 = &MEM[base: a_2(D), index: ivtmp_3, step: 8, offset: 0B];
		 .MASK_STORE (_1, 64B, loop_mask_4, vect__1.2);

		_1 = (sizetype) a_2(D);
		 vect_patt_3.3 = .MASK_GATHER_LOAD (_1, vect__4.4, 8,
						    { 0.0, ... }, loop_mask_5);
	      */
	      tree op1 = gimple_call_arg (stmt, 0);
	      if (TREE_CODE (op1) != SSA_NAME)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "get_references_in_gimple_call: ");
		      fprintf (dump_file, "find base that not ssa_name: ");
		      print_generic_expr (dump_file, op1, TDF_LINENO);
		      fprintf (dump_file, "\n");
		    }
		  return;
		}
	      gimple *op1_def = SSA_NAME_DEF_STMT (op1);
	      if (op1_def != NULL && gimple_code (op1_def) == GIMPLE_ASSIGN)
		{
		  /* &MEM[base: xx]  */
		  tree rhs1 = gimple_assign_rhs1 (op1_def);
		  /* If the definition stmt of the operation is memory
		     reference type, read it directly.  */
		  if (TREE_CODE (rhs1) == ADDR_EXPR
		      && TREE_CODE (TREE_OPERAND (rhs1, 0)) == TARGET_MEM_REF)
		    op1 = TREE_OPERAND (rhs1, 0); /* MEM[base: xx]  */
		}

	      add_ref (references, op1, stmt, true, read_p);
	      return;
	    }
	  default:
	    return;
	}
    }
}

/* Stores the locations of memory references in STMT to REFERENCES.  */

void
get_references_in_stmt (gimple *stmt, vector<data_ref> &references)
{
  if (!gimple_vuse (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "gimple_vuse: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_LINENO);
    }

  if (gimple_code (stmt) == GIMPLE_ASSIGN)
    {
      tree op0 = gimple_assign_lhs (stmt);
      tree op1 = gimple_assign_rhs1 (stmt);
      tree base = NULL_TREE;

      /* _1 = MEM[base: a, index: i, step: 8, offset: 0B];  */
      if (REFERENCE_CLASS_P (op1)  && (base = get_base_address (op1))
	  && TREE_CODE (base) != SSA_NAME && !is_gimple_min_invariant (base))
	add_ref (references, op1, stmt, false, true);

      if (REFERENCE_CLASS_P (op0) && get_base_address (op0))
	add_ref (references, op0, stmt, false, false);
    }
  else if (gimple_code (stmt) == GIMPLE_CALL)
    get_references_in_gimple_call (stmt, references);

  return;
}

/* flag of loop filter out.  */

struct loop_filter_out_flag
{
  /* Use external call.  */
  bool use_ext_call;

  /* Use external node.  */
  bool use_ext_node;

  /* Use loop defined in macros.  */
  bool use_macro_loop;

  /* Use external node.  */
  bool use_cond_func;
};

/* Check whether an external node is used.  */

bool use_ext_node_p (const vector<data_ref> &references,
		     unsigned int &start)
{
  expanded_location cfun_xloc
	= expand_location (DECL_SOURCE_LOCATION (current_function_decl));

  unsigned i = start;
  start = references.size ();
  for (; i < references.size (); i++)
    {
      data_ref ref = references[i];
      expanded_location xloc = expand_location (ref.stmt->location);
      if (xloc.file && filename_cmp (cfun_xloc.file, xloc.file))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "use_ext_node\n\n");
	  return true;
	}
    }
  return false;
}

/* Determine whether to filter out loops by stmt.  */

bool
filter_out_loop_by_stmt_p (loop_filter_out_flag &loop_filter, gimple *stmt,
		  const vector<data_ref> &references, unsigned int &start)
{
  expanded_location xloc = expand_location (stmt->location);
  /* check use_ext_call.  */
  if (gimple_code (stmt) == GIMPLE_CALL && !gimple_call_internal_p (stmt))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "use_ext_call: ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_LINENO);
	}
      loop_filter.use_ext_call = true;
      return true;
    }

  /* check use_macro_loop.  */
  if (xloc.file && xloc.column != 1)
    loop_filter.use_macro_loop = false;

  /* checke use_cond_func, VEC_COND_EXPR/MIN_EXPR/MAX_EXPR.  */
  if (gimple_code (stmt) == GIMPLE_ASSIGN)
    {
      enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
      if (rhs_code == VEC_COND_EXPR || rhs_code == MIN_EXPR
	  || rhs_code == MAX_EXPR)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "use_cond_func: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_LINENO);
	    }
	  loop_filter.use_cond_func = true;
	  return true;
	}
    }

  /* check use_ext_node.  */
  if (use_ext_node_p (references, start))
    {
      loop_filter.use_ext_node = true;
      return true;
    }

  return false;
}

/* Dump the flag type of the loop is filtered out.  */

void
dump_loop_filter_out_flag (loop_filter_out_flag &loop_filter)
{
  if (loop_filter.use_ext_call)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "non-dense mem access: use_ext_call\n");
    }

  if (loop_filter.use_ext_node)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "non-dense mem access: use_ext_node\n");
    }

  if (loop_filter.use_macro_loop)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "non-dense mem access: use_macro_loop\n");
    }

  if (loop_filter.use_cond_func)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "non-dense mem access: use_cond_func\n");
    }
}

/* Get references in loop.  */

bool
get_references_in_loop (vector<data_ref> &references,
			loop_filter_out_flag &loop_filter,
			class loop *loop)
{
  unsigned int start = 0;
  bool filter_out_loop = true;

  /* Analyze each bb in the loop.  */
  basic_block *body = get_loop_body_in_dom_order (loop);
  for (unsigned i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = body[i];
      if (bb->loop_father != loop)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n==== the %dth loop bb body ====\n", i);
	  gimple_dump_bb (dump_file, bb, 0, dump_flags);
	  fprintf (dump_file, "\n");
	}

      gimple_stmt_iterator bsi;
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  get_references_in_stmt (stmt, references);
	  filter_out_loop = filter_out_loop_by_stmt_p (loop_filter, stmt,
						       references, start);
	  if (filter_out_loop)
	    break;
	}
      if (filter_out_loop)
	break;
    }
  free (body);
  return !filter_out_loop;
}

/* Computes an estimated number of insns in LOOP, weighted by WEIGHTS.
   Assume that the HPC data reading and calculation process does not involve
   adding branches in loops.  Therefore, all bbs of loops are directly used for
   calculation (excluding embedded loops) without considering branch weighting.
*/

unsigned
estimate_loop_insns (class loop *loop, eni_weights *weights)
{
  basic_block *body = get_loop_body (loop);
  gimple_stmt_iterator gsi;
  unsigned size = 0, i;

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = body[i];
      if (bb->loop_father != loop)
	{
	  continue;
	}
      for (gsi = gsi_start_bb (body[i]); !gsi_end_p (gsi); gsi_next (&gsi))
	size += estimate_num_insns (gsi_stmt (gsi), weights);
    }
  free (body);

  return size;
}

/* Check whether the memory access is dense.  */

bool
dense_memory_p (const vector<data_ref> &references, class loop *loop)
{
  int ref_count = references.size ();
  unsigned int ninsns = estimate_loop_insns (loop, &eni_size_weights);
  float mem_to_insn_ratio = (float)ref_count / (float)ninsns;

  /* The number of cores to be run and DDR bandwidth information can be
  transferred to flexibly adjust the threshold.  */
  bool dense_mem = (mem_to_insn_ratio >= (param_mem_access_ratio / 100.0)
		    && ref_count >= param_mem_access_num);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      const char *fn_name = IDENTIFIER_POINTER (DECL_NAME (cfun->decl));

      /* Dump dense memory source code location.  */
      if (ref_count && references[0].stmt->location)
	{
	  expanded_location xloc = expand_location
				     (references[0].stmt->location);
	  int fn_start = 0;
	  if (DECL_SOURCE_LOCATION (current_function_decl))
	    fn_start = expand_location (
			    DECL_SOURCE_LOCATION (current_function_decl)).line;
	  int fn_end = fn_start;
	  if (cfun->function_end_locus)
	    fn_end = expand_location (cfun->function_end_locus).line;
	  if (xloc.file)
	    fprintf (dump_file, "[%s:%s(%d-%d):%d:%d] ",
		      xloc.file, fn_name, fn_start, fn_end,
		      xloc.line, xloc.column);
	}

      /* Dump memory dense information.  */
      if (dense_mem)
	fprintf (dump_file, "dense memory access: ");
      else
	fprintf (dump_file, "non-dense mem access: ");
      fprintf (dump_file,
	       "ref_count = %d, ninsns = %d, mem_to_insn_ratio = %f\n\n",
	       ref_count, ninsns, mem_to_insn_ratio);
    }

  return dense_mem;
}

/* Analyze the inner loop and get the loop with dense memory access.  */

void
analyze_loop_dense_memory (vector<class loop *> &kernels,
			  map<class loop *, vector<data_ref> > &kernels_refs,
			  class loop *loop)
{
  vector<data_ref> references;
  number_of_latch_executions (loop);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\n========== Processing loop %d: ==========\n",
	      loop->num);
      loop_dump (dump_file, loop);
      flow_loop_dump (loop, dump_file, NULL, 1);
      fprintf (dump_file, "loop unroll: %d\n", loop->unroll);
    }

  if (get_loop_exit_edges (loop).length () != 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "non-dense mem access: loop_branching\n");
      return;
    }

  loop_filter_out_flag loop_filter = {false, false, true, false};

  if (!get_references_in_loop (references, loop_filter, loop))
    {
      dump_loop_filter_out_flag (loop_filter);
      return;
    }

  if (dense_memory_p (references, loop))
    {
      kernels_refs[loop] = references;
      kernels.push_back (loop);
    }
}
/* Analyze the inner loop and get the loop with dense memory access.  */

bool
get_dense_memory_kernels (vector<class loop *> &kernels,
			  map<class loop *, vector<data_ref> > &kernels_refs)
{
  if (dump_file)
    fprintf (dump_file, "\nPhase 1: get_dense_memory_kernels\n\n");
  class loop *loop = NULL;
  FOR_EACH_LOOP (loop, LI_ONLY_INNERMOST)
    analyze_loop_dense_memory (kernels, kernels_refs, loop);
  return kernels.size () > 0;
}

/* ================ phase 2 trace_data_refs_info ================  */

/* Determine whether the declaration is a non-vectorized.  */

bool
generic_decl_p (tree expr)
{
  if (expr == NULL_TREE)
    return false;
  enum tree_code expr_code = TREE_CODE (expr);
  if (expr_code != VAR_DECL && expr_code != PARM_DECL
      && expr_code != COMPONENT_REF)
    return false;

  tree type = TREE_TYPE (expr);
  while (type)
    {
      if (TREE_CODE (type) != VECTOR_TYPE)
	/* TREE_TYPE (NODE) (
		      CONTAINS_STRUCT_CHECK (NODE, TS_TYPED)->typed.type)  */
	type = CONTAINS_STRUCT_CHECK (type, TS_TYPED) ? TREE_TYPE (type) : NULL;
      else
	return false;
    }
  return true;
}

/* Initial worklist preparation for source variable tracing.
   Add different initial node based on different gimple statements.  */

void
add_worklist (vector<tree> &worklist, set<tree> &walked, gimple *def_stmt)
{
  if (gimple_code (def_stmt) == GIMPLE_PHI)
    {
      for (unsigned i = 0; i < gimple_phi_num_args (def_stmt); i++)
	{
	  tree node = gimple_phi_arg_def (def_stmt, i);
	  if (!walked.count (node))
	    {
	      worklist.push_back (node);
	      walked.insert (node);
	    }
	}
    }
  else if (is_gimple_assign (def_stmt))
    {
      tree_code rhs_code = gimple_assign_rhs_code (def_stmt);
      if (rhs_code == POINTER_PLUS_EXPR || rhs_code == NEGATE_EXPR
	  || rhs_code == NOP_EXPR || rhs_code == SSA_NAME
	  || rhs_code == COMPONENT_REF)
	{
	  tree node = gimple_assign_rhs1 (def_stmt);
	  if (!walked.count (node))
	    {
	      worklist.push_back (node);
	      walked.insert (node);
	    }
	}
      else if (rhs_code == PLUS_EXPR || rhs_code == MINUS_EXPR)
	{
	  tree node = gimple_assign_rhs1 (def_stmt);
	  if (!walked.count (node))
	    {
	      worklist.push_back (node);
	      walked.insert (node);
	    }
	  node = gimple_assign_rhs2 (def_stmt);
	  if (!walked.count (node))
	    {
	      worklist.push_back (node);
	      walked.insert (node);
	    }
	}
      else
	{
	  /* unhandled assign rhs_code: _219 = _17 * _70;
	     _17 = *grid_56(D).sst.span;
	     _70 = *grid_56(D).sst.dim[0].stride;
	  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "unhandled assign rhs_code: ");
	      print_gimple_stmt (dump_file, def_stmt, 0, TDF_LINENO);
	      fprintf (dump_file, "\n");
	    }
	}
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "unsupported tracing stmt: ");
	  print_gimple_stmt (dump_file, def_stmt, 0, TDF_LINENO);
	  fprintf (dump_file, "\n");
	}
    }
}


/* Tracing source variables:
   vectp.1 = a_2(D) + _3;
   _4 = &MEM[base: vectp.1, index: ivtmp_5, step: 8, offset: 0B];
   vect__1.6 = .MASK_LOAD (_4, 64B, loop_mask_7);

   _1 = (sizetype) b_2(D);
   vect_patt_3.3 = .MASK_GATHER_LOAD (_1, vect__4.4, 8, { 0.0, ... },
				      loop_mask_5);
  ...
  Due to previous pass optimizations, the current tracing method can find
  several source variable candidates.  We decide to record them in a map and
  later filter out the true base variable by some criteria.
*/

void
trace_base_var_helper (tree arg, set<tree> &walked,
		       map<tree, int>& base_var_candid)
{
  if (arg == NULL)
    return;

  /* Array type.  */
  tree op0 = NULL;
  if (TREE_CODE (arg) == ADDR_EXPR
      && (op0 = TREE_OPERAND (arg, 0)) && generic_decl_p (op0))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "array type\n");
      base_var_candid[op0] += 1;
      return;
    }

  /* Pointer type.  */
  if (TREE_CODE (TREE_TYPE (arg)) == POINTER_TYPE && generic_decl_p (arg))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "pointer type\n");
      base_var_candid[arg] += 1;
      return;
    }

  /* SSA_NAME type.  */
  if (TREE_CODE (arg) != SSA_NAME)
    return;

  tree tmp_var = SSA_NAME_VAR (arg);
  if (tmp_var && generic_decl_p (tmp_var)
      && TREE_CODE (TREE_TYPE (arg)) == POINTER_TYPE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "ssa pointer type\n");
      base_var_candid[tmp_var] += 1;
      return;
    }

  gimple *def_stmt = SSA_NAME_DEF_STMT (arg);
  if (def_stmt == NULL)
    return;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_generic_expr (dump_file, arg, TDF_SLIM);
      fprintf (dump_file, "\t\t: ");
      print_gimple_stmt (dump_file, def_stmt, 0, TDF_SLIM);
    }

  vector<tree> worklist;
  add_worklist (worklist, walked, def_stmt);
  for (unsigned i = 0; i < worklist.size (); ++i)
    trace_base_var_helper (worklist[i], walked, base_var_candid);
}

/* Identify the base variable traced from base address of memory reference.
   We recognize that current method could detect several base variable
    candidates and the temporary criteria for base variable determination
    is that either one of the following statement is true:
      1. The number of base variable candidates is 1;
      2. The number of detected gimple statements for some variable is 1.
    We may use other criteria or relax the current criteria
    (e.g., criterion 2: 1 -> any odd number).    */

bool
trace_base_var (tree &var, tree arg, set<tree> &walked)
{
  map<tree, int> base_var_candid;
  trace_base_var_helper (arg, walked, base_var_candid);
  bool is_tracing_unusual = false;
  if (base_var_candid.size () == 1)
    var = base_var_candid.begin ()->first;
  else
    {
      is_tracing_unusual = true;
      for (const pair<tree, int>& base_var_count : base_var_candid)
	if (base_var_count.second == 1)
	  var = base_var_count.first;
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Traced variables at ");
      print_generic_expr (dump_file, arg, TDF_SLIM);
      fprintf (dump_file, ":\n");
      for (const pair<tree, int>& base_var_count : base_var_candid)
	fprintf (dump_file, "%s:%d, ", get_name (base_var_count.first),
		  base_var_count.second);
      fprintf (dump_file, "\n");

      if (var == NULL_TREE)
	fprintf (dump_file, "Unhandled scenario for tracing base variable.\n");
      else if (is_tracing_unusual && var != NULL_TREE)
	fprintf (dump_file, "Tracing unusual number or occurrences of base "
		"variables.  Choose %s.\n", get_name (var));
    }
  return var != NULL_TREE;
}

/* Tracing direct memory reference information.  */

bool
trace_direct_mem_ref (data_ref &mem_ref, set <gimple *> &traced_ref_stmt)
{
  if (TREE_CODE (mem_ref.ref) != TARGET_MEM_REF)
    return false;

  /* Direct memory access, regardless of whether it is in vectorized form,
     can be determined through TARGET_MEM_REF.  */
  mem_ref.base = TREE_OPERAND (mem_ref.ref, 0);
  mem_ref.offset = TREE_OPERAND (mem_ref.ref, 1);
  mem_ref.index = TREE_OPERAND (mem_ref.ref, 2);
  mem_ref.step = TREE_OPERAND (mem_ref.ref, 3);

  set<tree> walked;
  if (mem_ref.var == NULL_TREE
      && !trace_base_var (mem_ref.var, mem_ref.base, walked))
    return false;

  traced_ref_stmt.insert (mem_ref.stmt);
  return true;
}

/* Recursively trace and check whether the definition stmt of the
   index operand is a recorded stmt in direct access tracing.
   If true, it is an indirect access.  */

bool
trace_indirect_operand (tree arg, set<gimple *> &traced_ref_stmt)
{
  if (TREE_CODE (arg) != SSA_NAME)
    return false;

  gimple *def_stmt = SSA_NAME_DEF_STMT (arg);

  if (traced_ref_stmt.count (def_stmt))
    return true;

  if (!def_stmt || !is_gimple_assign (def_stmt))
    return false;

  tree_code rhs_code = gimple_assign_rhs_code (def_stmt);
  /* Collect a whitelist of gimple_assign_rhs_code for tracing pointer/array
     type indirect memory access.  Please check examples before function
     trace_indirect_ptr and trace_indirect_array.  */
  if (rhs_code != MULT_EXPR && rhs_code != NOP_EXPR
      && rhs_code != CONVERT_EXPR && rhs_code != PLUS_EXPR
      && rhs_code != ARRAY_REF)
    return false;

  tree op = NULL_TREE;
  ssa_op_iter iter;
  FOR_EACH_SSA_TREE_OPERAND (op, def_stmt, iter, SSA_OP_USE)
    {
      if (trace_indirect_operand (op, traced_ref_stmt))
	return true;
    }
  return false;
}

/* Trace the pointer of the indirect memory access:
   1) obtain the base address of the indirect memory access.
   2) ensure that the index has been traced in the direct memory access.

   _1 = MEM[base: a_2(D), index: ivtmp.3_3, step: 4, offset: 0B]; // Traced in
   direct access
   _4 = (long unsigned int) _1;
   _5 = _4 * 8;
   _6 = p(D) + _5; // get base
   _7 = *_6;       // start tracing
*/

bool
trace_indirect_ptr (tree &base, tree &index, tree arg,
		    set<gimple *> traced_ref_stmt)
{
  gimple *def_stmt = SSA_NAME_DEF_STMT (arg);

  if (!def_stmt || !is_gimple_assign (def_stmt))
    return false;

  tree_code rhs_code = gimple_assign_rhs_code (def_stmt);
  if (rhs_code != POINTER_PLUS_EXPR)
    return false;

  /* POINTER_PLUS_EXPR, The first operand is always a pointer/reference type.
     The second operand is always an unsigned integer type compatible with
     sizetype.  */
  base = gimple_assign_rhs1 (def_stmt);
  index = gimple_assign_rhs2 (def_stmt);

  return trace_indirect_operand (index, traced_ref_stmt);
}

/* Trace the array of the indirect memory access:
   1) obtain the base address of the indirect memory access.
   2) ensure that the index has been traced in the direct memory access.

   _1 = MEM[base: a_2(D), index: ivtmp.3_3, step: 4, offset: 0B]; // Traced in
   direct access
   _4 = (integer(kind=8)) _1;
   _5 = _4 + 135;
   _6 = p[_5];       // start tracing
*/

bool
trace_indirect_array (tree &base, tree &index,
		      set<gimple *> traced_ref_stmt, tree ref)
{
  if (TREE_CODE (ref) != ARRAY_REF)
    return false;
  base = TREE_OPERAND (ref, 0);
  index = TREE_OPERAND (ref, 1);
  return trace_indirect_operand (index, traced_ref_stmt);
}

/* Tracing indirect memory reference information.
   Include tracing of base addresses and source variable.
   _x(ssa name) -> a_2(base addr) -> a(src var)  */

bool
trace_indirect_mem_ref (data_ref &mem_ref,
			set <gimple *> &traced_ref_stmt)
{
  /* Processing of vectorization types.  */
  if (mem_ref.vectorize_p)
    {
      tree op = gimple_call_arg (mem_ref.stmt, 1);
      if (trace_indirect_operand (op, traced_ref_stmt))
	{
	  mem_ref.base = gimple_call_arg (mem_ref.stmt, 0);
	  mem_ref.regular_p = false;
	  set<tree> walked;
	  if (mem_ref.var == NULL_TREE
	      && !trace_base_var (mem_ref.var, mem_ref.base, walked))
	    return false;
	  return true;
	}
      return false;
    }

 /* Processing of non-vectorized types.  */
  tree op = NULL_TREE;
  ssa_op_iter iter;
  FOR_EACH_SSA_TREE_OPERAND (op, mem_ref.stmt, iter, SSA_OP_USE)
    {

      /* Array type:
	 _1 = MEM[base: a_2(D), index: ivtmp.3_3, step: 4, offset: 0B];
	 _4 = c[_1];

	 Pointer type:
	 _1 = MEM[base: a_2(D), index: ivtmp.3_3, step: 4, offset: 0B];
	 _4 = (long unsigned int) _1;
	 _5 = _4 * 8;
	 _6 = p(D) + _5;
	 _7 = *_6;
      */
      tree base = NULL_TREE;
      tree index = NULL_TREE;
      if (trace_indirect_array (base, index, traced_ref_stmt, mem_ref.ref)
	  || trace_indirect_ptr (base, index, op, traced_ref_stmt))
	{
	  /* ARRAY_REF, The first operand is the array;
			the second is the index.  */
	  mem_ref.base = base;
	  mem_ref.index = index;
	  mem_ref.regular_p = false;
	  set<tree> walked;
	  if (mem_ref.var == NULL_TREE
	      && !trace_base_var (mem_ref.var, mem_ref.base, walked))
	    return false;
	  return true;
	}
    }

  return false;
}

/* Trace references base info:
   1) Parallel analysis
   2) Memory access rule analysis
   3) Tracing base address and source variable of memory references
   We will extend parallel analysis later.
*/

void
trace_ref_info (data_ref &mem_ref, set <gimple *> &traced_ref_stmt)
{
  enum tree_code ref_code = TREE_CODE (mem_ref.ref);
  if (/* Vectorized and non-vectorized direct access.  */
      ref_code != TARGET_MEM_REF
      /* non-vectorized indirect memory access.  */
      && ref_code != MEM_REF && ref_code != ARRAY_REF
      /* vectorized indirect memory access.  */
      && ref_code != SSA_NAME)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "ref is another tree-code: ");
	  fprintf (dump_file, "stmt: ");
	  print_gimple_stmt (dump_file, mem_ref.stmt, 0, TDF_LINENO);
	  fprintf (dump_file, "ref: ");
	  print_generic_expr (dump_file, mem_ref.ref, TDF_LINENO);
	  fprintf (dump_file, "\n");
	}
      return;
    }

  /* 1) Direct and indirect access traces and traces source variables.  */
  if (!trace_direct_mem_ref (mem_ref, traced_ref_stmt)
      && !trace_indirect_mem_ref (mem_ref, traced_ref_stmt))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Tracing failed.\n\n");
      return;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Tracing succeeded.\n\n");
  mem_ref.trace_status_p = true;
}

/* Trace all references in the loop.  */

void
trace_loop_refs_info (vector<data_ref> &refs, set <gimple *> &traced_ref_stmt)
{
  for (unsigned i = 0; i < refs.size (); ++i)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "trace_references_base_info %d:\n", i);
	  print_generic_expr (dump_file, refs[i].ref, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      trace_ref_info (refs[i], traced_ref_stmt);
    }
}

/* Tracing and sorting reference groups.  */

void
trace_data_refs_info (vector<class loop *> &kernels,
		      map<class loop*, vector<data_ref> > &loop_refs,
		      set <gimple *> &traced_ref_stmt)
{
  if (dump_file)
    fprintf (dump_file, "\nPhase 2: trace_all_references_info\n\n");

  for (unsigned i = 0; i < kernels.size (); ++i)
    {
      class loop *loop = kernels[i];
      if (loop_refs.count (loop) == 0)
	continue;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "loop header %d:\n", loop->header->index);
      trace_loop_refs_info (loop_refs[loop], traced_ref_stmt);
    }
}

/* ================ phase 3 analyze_nested_kernels ================  */

/* Return the inner most type for arrays and pointers of TYPE.  */

tree
inner_type (tree type)
{
  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);
  return type;
}

/* Check whether the input iv is the loop dimension boundary.  */

bool
loop_bound_iv_p (tree t, tree &outer_loop_t)
{
  if (t == NULL || TREE_CODE (t) != SSA_NAME
      || TREE_CODE (TREE_TYPE (t)) != INTEGER_TYPE)
  return false;

  gimple *def_stmt = SSA_NAME_DEF_STMT (t);
  if (gimple_code (def_stmt) != GIMPLE_PHI)
    return false;

  /* Filter scenarios with only two phi inputs.  */
  if (gimple_phi_num_args (def_stmt) != 2)
    return false;

  gphi *phi_stmt = as_a <gphi *> (def_stmt);
  basic_block src0 = gimple_phi_arg_edge (phi_stmt, 0)->src;
  basic_block src1 = gimple_phi_arg_edge (phi_stmt, 1)->src;

  class loop *loop = loop_containing_stmt (def_stmt);
  bool res = false;
  /* Two phi inputs, one from the current loop and one from the outer loop.  */
  if ((src0->loop_father == loop) && (src1->loop_father == loop_outer (loop)))
    {
      outer_loop_t = gimple_phi_arg_def (def_stmt, 1);
      res = true;
    }
  else if ((src1->loop_father == loop)
	   && (src0->loop_father == loop_outer (loop)))
    {
      outer_loop_t = gimple_phi_arg_def (def_stmt, 0);
      res = true;
    }

  if (res)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "===> ");
	  print_gimple_stmt (dump_file, def_stmt, 0, TDF_SLIM);
	}
      return true;
    }
  return false;
}

/* add worklist and walked list.  */

void
add_worklist_walked (vector<tree> &worklist, set<tree> &walked, tree node)
{
  if (!walked.count (node))
    {
      worklist.push_back (node);
      /* Avoid phi node cycle introduction, which makes the worklist unable
	 to end.  */
      walked.insert (node);
    }
}

/* check bound iv and add worklist.  */

void
check_bound_iv_and_add_worklist (vector<tree> &worklist, set<tree> &walked,
				 tree t, data_ref &mem_ref)
{
  if (t == NULL_TREE || TREE_CODE (t) != SSA_NAME)
    return;

  gimple *def_stmt = SSA_NAME_DEF_STMT (t);
  if (def_stmt == NULL)
    return;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_generic_expr (dump_file, t, TDF_SLIM);
      fprintf (dump_file, "\t\t: ");
      print_gimple_stmt (dump_file, def_stmt, 0, TDF_SLIM);
    }

  if (gimple_code (def_stmt) == GIMPLE_PHI)
    {
      tree out_loop_t = NULL_TREE;
      if (loop_bound_iv_p (t, out_loop_t))
	{
	  mem_ref.loop_bounds.push_back (loop_bound (t, def_stmt));
	  add_worklist_walked (worklist, walked, out_loop_t);
	}
    }
  else if (is_gimple_assign (def_stmt))
    {
      tree_code rhs_code = gimple_assign_rhs_code (def_stmt);

      /* unary.  */
      if (rhs_code == SSA_NAME || rhs_code == NOP_EXPR)
	add_worklist_walked (worklist, walked, gimple_assign_rhs1 (def_stmt));
      else if (rhs_code == POINTER_PLUS_EXPR)
	add_worklist_walked (worklist, walked, gimple_assign_rhs2 (def_stmt));

      /* binary.  */
      else if (rhs_code == PLUS_EXPR || rhs_code == MINUS_EXPR
	       || rhs_code == MULT_EXPR)
	{
	  add_worklist_walked (worklist, walked, gimple_assign_rhs1 (def_stmt));
	  add_worklist_walked (worklist, walked, gimple_assign_rhs2 (def_stmt));
	}
    }
}

/* DFS trace the loop bound of iv.  */

bool
trace_loop_bound_iv (data_ref &mem_ref)
{
  /* Indirect memory access, the size cannot be determined based on the loop
     boundary.  */
  if (!mem_ref.regular_p)
    return false;

  /* Determine and record the boundary iv of the current index,
     but do not trace it.  */
  tree outer_loop_t = NULL_TREE;
  if (loop_bound_iv_p (mem_ref.index, outer_loop_t))
    mem_ref.loop_bounds.push_back (
	    loop_bound (mem_ref.index, SSA_NAME_DEF_STMT (mem_ref.index)));

  vector<tree> worklist;
  worklist.push_back (mem_ref.base);
  set<tree> walked;

  while (worklist.size ())
    {
      tree t = worklist.back ();
      worklist.pop_back ();

      /* add worklist.  */
      check_bound_iv_and_add_worklist (worklist, walked, t, mem_ref);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nmem_ref access dimension: %ld\n",
	      mem_ref.loop_bounds.size ());
      fprintf (dump_file, "Traced variables: ");
      print_generic_expr (dump_file, mem_ref.base, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  return mem_ref.loop_bounds.size () > 0;
}

/* dump loop bound.  */

void
loop_bound_dump (FILE *file, loop_bound &lb)
{
  class loop *loop = lb.loop;
  fprintf (file, "loop_bound: loop_%d (", loop->num);
  if (loop->header)
    fprintf (file, "header = %d", loop->header->index);
  else
    {
      fprintf (file, "deleted)\n");
      return;
    }
  if (loop->latch)
    fprintf (file, ", latch = %d", loop->latch->index);
  fprintf (file, ", lb_niters = ");
  print_generic_expr (file, lb.niters);
  fprintf (file, ")\n");
}

/* static calculate data size.  */

void
static_calculate_data_size (data_ref &mem_ref)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nstatic_calculate_data_size\n");

  tree size_unit = TYPE_SIZE_UNIT (inner_type (TREE_TYPE (mem_ref.var)));
  HOST_WIDE_INT type_size = size_unit ? tree_to_uhwi (size_unit) : 0;
  for (unsigned i = 0; i < mem_ref.loop_bounds.size (); ++i)
    {
      HOST_WIDE_INT est_niter = tree_to_uhwi (mem_ref.loop_bounds[i].niters);
      unsigned int unroll = mem_ref.loop_bounds[i].unroll;
      if (i == 0)
	{
	  /* The unit conversion between byte, kilobytes, and megabytes is
	     1024.  */
	  mem_ref.data_size = double (type_size
				      * est_niter * unroll) / 1024 / 1024;
	}
      else
	mem_ref.data_size *= est_niter * unroll;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "static_data_size: %lf\n", mem_ref.data_size);
    }
}

/* Recursive tracing and creating of dominant nodes.  */

tree
trace_and_create_dominate_expr (tree expr, class loop *outermost)
{
  if (expr == NULL_TREE || is_gimple_constant (expr))
    return expr;

  if (TREE_CODE (expr) != SSA_NAME)
    return NULL_TREE;

  if (SSA_NAME_IS_DEFAULT_DEF (expr))
    return expr;

  gimple *stmt = SSA_NAME_DEF_STMT (expr);
  basic_block def_bb = gimple_bb (stmt);
  if (def_bb == NULL || def_bb->loop_father == NULL)
    return NULL_TREE;

  if (dominated_by_p (CDI_DOMINATORS, outermost->header, def_bb))
    return expr;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return NULL_TREE;

  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree_code_class code_class = TREE_CODE_CLASS (rhs_code);
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  tree rhs1 = trace_and_create_dominate_expr
		(gimple_assign_rhs1 (stmt), outermost);
  if (rhs1 == NULL_TREE)
    return NULL_TREE;

  if (code_class == tcc_unary)
    {
      tree expr_new = build1 (rhs_code, type, rhs1);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "expr_new = ");
	  print_generic_expr (dump_file, expr_new, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      return expr_new;
    }
  else if (code_class == tcc_binary)
    {
      tree rhs2 = trace_and_create_dominate_expr
		    (gimple_assign_rhs2 (stmt), outermost);
      if (rhs2 == NULL_TREE)
	return NULL_TREE;

      tree expr_new = fold_build2 (rhs_code, type, rhs1, rhs2);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "expr_new = ");
	  print_generic_expr (dump_file, expr_new, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      return expr_new;
    }

  return NULL_TREE;
}

/* Recursive parsing and craating of nodes in expr expressions.  */

tree
parse_and_create_expr (tree expr, class loop *outermost)
{
  if (expr == NULL_TREE || expr == chrec_dont_know
      || is_gimple_constant (expr) || TREE_CODE (expr) == ADDR_EXPR)
    {
      /* tcc_expression (e.g., &q) situation combined with tcc_unary.  */
      if (TREE_CODE (expr) == ADDR_EXPR && dump_file
	  && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "tcc_expression case in ADDR_EXPR: ");
	  print_generic_expr (dump_file, expr, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      return expr;
    }

  if (TREE_CODE (expr) == SSA_NAME)
    return trace_and_create_dominate_expr (expr, outermost);
  else if (EXPR_P (expr))
    {
      enum tree_code tree_code = TREE_CODE (expr);
      tree_code_class code_class = TREE_CODE_CLASS (tree_code);
      tree type = TREE_TYPE (expr);
      tree op1 = parse_and_create_expr (TREE_OPERAND (expr, 0), outermost);
      if (op1 == NULL_TREE)
	return NULL_TREE;

      if (code_class == tcc_unary)
	{
	  tree expr_new = build1 (tree_code, type, op1);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "expr_new = ");
	      print_generic_expr (dump_file, expr_new, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	  return expr_new;
	}
      else if (code_class == tcc_binary)
	{
	  tree op2 = parse_and_create_expr (TREE_OPERAND (expr, 1), outermost);
	  if (op2 == NULL_TREE)
	    return NULL_TREE;

	  tree expr_new = fold_build2 (tree_code, type, op1, op2);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "expr_new = ");
	      print_generic_expr (dump_file, expr_new, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	  return expr_new;
	}
    }
  return NULL_TREE;
}

/* Trace and creat dominate loop bounds.  */

void
trace_and_create_dominate_loop_bounds (data_ref &mem_ref)
{
  /* Check whether the niters is a loop dominant.
     If not, trace and determine whether the result is dominant.  If yes, create
     the expr of the dominant node.
  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\ntrace_and_create_dominate_loop_bounds\n");

  /* Determine the relationship between the boundary of the innermost loop and
     the dominant of the outer loop and the processing.  */
  loop_bound &outermost = mem_ref.loop_bounds.back ();
  for (unsigned i = 0; i < mem_ref.loop_bounds.size (); ++i)
    {
      loop_bound &current = mem_ref.loop_bounds[i];
      tree &niters = current.niters;
      if (TREE_CODE (niters) == COND_EXPR)
	niters = TREE_OPERAND (niters, 1);

      niters = parse_and_create_expr (niters, outermost.loop);

      if (niters == NULL_TREE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      print_generic_expr (dump_file, mem_ref.ref, TDF_SLIM);
	      fprintf (dump_file, "Tracing loop bound failed at dimension %d\n",
		       i);
	    }
	  mem_ref.calc_by = UNHANDLE_CALC;
	  break;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	loop_bound_dump (dump_file, mem_ref.loop_bounds[i]);
    }
}

/* trace the dimension and corresponding loop bounds of mem_ref.
   This function is used to supplement the information of mem_ref.loop_bounds.
*/

void
trace_ref_dimension_and_loop_bounds (data_ref &mem_ref)
{
  /* In the same loop, some memory access dimensions are different.  Remove
     variables with fewer dimensions.
     Previous cyclic filtering conditions and memory access node records and
     tracing.
     The false result is also processed.
  */
  if (dump_file)
    fprintf (dump_file, "\ncalculate_data_size\n");

  /* Trace the loop bound iv of ref to determine the dimension.  */
  /* Record data from the loop perspective to avoid repeated tracing.  */
  if (!trace_loop_bound_iv (mem_ref))
    return;

  /* The traced mem_ref may have multiple dimensions, which corresponds to
     multiple loops.  */
  /* And in the dimension-by-dimensional analysis, the computable way is
     continuously reduced.  */
  mem_ref.calc_by = STATIC_CALC;
  for (unsigned i = 0; i < mem_ref.loop_bounds.size (); ++i)
    {
      class loop *loop = mem_ref.loop_bounds[i].loop;
      tree &niters = mem_ref.loop_bounds[i].niters;

      /* Set NULL_TREE to ensure that nb_iterations are retraced and
	 vec_nb_iterations are also extracted.  */
      loop->nb_iterations = NULL_TREE;
      niters = number_of_latch_executions (loop, false);
      if (dump_file && (dump_flags & TDF_DETAILS))
	loop_dump (dump_file, loop);

      if (loop->unroll)
	{
	  if (loop->unroll == USHRT_MAX && dump_file
	      && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "loop->unroll = USHRT_MAX = %d", USHRT_MAX);
	  mem_ref.loop_bounds[i].unroll = loop->unroll;
	}

      if ((niters == chrec_dont_know) && loop->vec_nb_iterations
	   && (loop->vec_nb_iterations != chrec_dont_know))
	niters = loop->vec_nb_iterations;
      if (dump_file && (dump_flags & TDF_DETAILS))
	loop_bound_dump (dump_file, mem_ref.loop_bounds[i]);

      if (niters == NULL_TREE || niters == chrec_dont_know)
	mem_ref.calc_by = min (mem_ref.calc_by, UNHANDLE_CALC);
      else if (TREE_CODE (niters) != INTEGER_CST)
	mem_ref.calc_by = min (mem_ref.calc_by, RUNTIME_CALC);
      else
	mem_ref.calc_by = min (mem_ref.calc_by, STATIC_CALC);
    }

  if (mem_ref.calc_by == RUNTIME_CALC)
    trace_and_create_dominate_loop_bounds (mem_ref);
  else if (mem_ref.calc_by == STATIC_CALC)
    static_calculate_data_size (mem_ref);
}

/* Get the loop's niters tree.
   Return NULL_TREE if not found.  */

tree
get_cur_loop_niters (map<class loop*, vector<data_ref> > &loop_refs,
		     class loop* loop)
{
  if (loop_refs.count (loop) == 0)
    return NULL_TREE;
  vector<loop_bound> bounds = loop_refs[loop][0].loop_bounds;
  return bounds.size () ? bounds[0].niters : NULL_TREE;
}

/* Trace the sources of the niters tree and return the
   outermost depth of the loops containing them.
   Return start_depth if not found.

   example:
   niters:(long) (((int) i_end_417 - (int) i_start_452) + 1)
   operand_num: 1, subtree:(long) (((int) i_end_417 - (int) i_start_452) + 1)
   operand_num: 2, subtree:((int) i_end_417 - (int) i_start_452) + 1
   operand_num: 2, subtree:(int) i_end_417 - (int) i_start_452
   operand_num: 1, subtree:(int) i_end_417
   SSA_NAME of niters: i_end_417
   gimple of SSA: i_end_417 = PHI <i_end_446(9), i_end_410(100)>
   return gimple depth;
*/

unsigned
trace_outer_loop_depth (tree niters, unsigned start_depth)
{
  /* If niter does not exist or the type is INTEGER_CST,
     the loop bound is determined and return start_depth.  */
  if (niters == NULL_TREE || TREE_CODE (niters) == INTEGER_CST)
    return start_depth;

  gimple *def_stmt = NULL;
  /* niters examples: i_start_452, fEnd_35, fEnd_100.  */
  enum tree_code niter_code = TREE_CODE (niters);
  if (niter_code == SSA_NAME)
    {
      /* Trace the SSA that define this niter.  */
      def_stmt = SSA_NAME_DEF_STMT (niters);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "ssa_name of niters: ");
	  print_generic_expr (dump_file, niters);
	  fprintf (dump_file, "\ngimple of ssa: \n");
	  print_gimple_stmt (dump_file, def_stmt, 0, TDF_LINENO);
	  fprintf (dump_file, "\n");
	}
      /* Termination condition of dfs.  Return the depth of the bb block.  */
      if (gimple_code (def_stmt) == GIMPLE_PHI
          || gimple_code (def_stmt) == GIMPLE_NOP)
	{
	  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (niters));
	  if (def_bb == NULL || def_bb->loop_father == NULL)
	    return start_depth;
	  unsigned ret_depth = loop_depth (def_bb->loop_father);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Stop tracing the outer loop depth, ");
	      fprintf (dump_file, "current depth: %d, current bb: %d\n",
				  ret_depth, def_bb->index);
	    }
	  return ret_depth;
	}
      /* 'ASSIGN': Use dfs to trace the rhs of the assignment statement.  */
      else if (gimple_code (def_stmt) == GIMPLE_ASSIGN)
	{
	  tree rhs = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (rhs) == TARGET_MEM_REF)
	    /* fEnd_35 = MEM[base: _19, index: ivtmp.96, step: 4,
			     offset: 0B]  */
	    return trace_outer_loop_depth (TREE_OPERAND (rhs, 2), start_depth);
	  else
	    {
	      /* M.218_658 = MIN_EXPR <_631, _657>  */
	      unsigned min_depth = start_depth;
	      unsigned operand_num = gimple_num_ops (def_stmt);
	      /* 'ASSIGN': start from 1 because op[0] is the lhs.  */
	      for (unsigned i = 1; i < operand_num; i++)
		{
		  tree subtree = dyn_cast<gassign *>(def_stmt)->op[i];
		  if (subtree == NULL)
		    continue;
		  unsigned depth = trace_outer_loop_depth (subtree, \
				   start_depth);
		  min_depth = MIN (min_depth, depth);
		  }
		return min_depth;
	    }
	}
      else
	{
	  /* Adding termination conditions:
	   1.  Niters is MEM variable;
	   2.  Niters is a runtime value (smooth_uPtr), and consider \
	       finding footprint in other mem_ref;
	   3.  Niters is loop variable (i_start/i_end), and the boundary in \
	       the outer loop depends on the variable j_start/j_end.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "The loop termination condition");
	      fprintf (dump_file, "is to be extended.\n");
	    }
	  return start_depth;
	}
    }
  /* The operand nums can be obtained when the tree code is as follows.  */
  else if (niter_code == NOP_EXPR || niter_code == MEM_REF
	   || niter_code == ARRAY_REF || niter_code == COND_EXPR
	   || niter_code == PLUS_EXPR || niter_code == MINUS_EXPR
	   || niter_code == TARGET_MEM_REF || niter_code == POINTER_PLUS_EXPR)
    {
      /* operand_num is the operand in the niters statement.
	 example: In the following niter statement, operand_num = 3.
	 (unsigned int) fEnd_35 - (unsigned int) fEnd_100 + 4294967295.  */
      unsigned operand_num = TREE_OPERAND_LENGTH (niters);
      unsigned min_depth = start_depth;
      for (unsigned i = 0; i < operand_num; i++)
	{
	  tree subtree = TREE_OPERAND (niters, i);
	  if (subtree == NULL)
	    continue;
	  unsigned depth = trace_outer_loop_depth (subtree, start_depth);
	  min_depth = MIN (min_depth, depth);
	}
      return min_depth;
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "niters is another tree code: %s\n", \
		   get_tree_code_name (niter_code));
	  print_generic_expr (dump_file, niters, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      return start_depth;
    }
}

/* Traces the ref dimension information in each loop.  */

void
analyze_loop_refs_dimension (vector<data_ref> &refs)
{
  for (unsigned i = 0; i < refs.size (); ++i)
    {
      if (refs[i].trace_status_p == false)
	continue;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "trace_reference_dimension %d:\n", i);
	  print_generic_expr (dump_file, refs[i].ref, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      trace_ref_dimension_and_loop_bounds (refs[i]);
    }
}
/* analyze nested kernels
   1. multidimension loop analyze
   2. extended outer loop analyze
*/

bool
analyze_nested_kernels (vector<class loop *> &kernels,
			map<class loop*, vector<data_ref> > &loop_refs,
			set <gimple *> &traced_ref_stmt)
{
  if (dump_file)
    fprintf (dump_file, "\nPhase 3: analyze_nested_kernels\n\n");

  /* `kernels` may be added in during outer loop extension phase,
     thus using initial size to avoid repeatedly analyzing.  */
  unsigned init_kernels_size = kernels.size ();
  for (unsigned i = 0; i < init_kernels_size; ++i)
    {
      class loop* loop = kernels[i];
      if (loop_refs.count (loop) == 0)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "loop header %d:\n", loop->header->index);
      analyze_loop_refs_dimension (loop_refs[loop]);

      unsigned depth = loop_depth (loop);
      unsigned outer_depth = trace_outer_loop_depth (get_cur_loop_niters \
			     (loop_refs, loop), depth);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "cur_depth: %d, outer_depth: %d\n", \
			    depth, outer_depth);
      /* param_outer_loop_num: number of loops of the extended outer loop.
	 Outermost loop should not be extended when outer_depth = 0.
	 `outer_depth == depth` means the current loop is the loop which
	 boundary is known, so there is no need to extend the outer loop.  */
      if (outer_depth == 0 || outer_depth == depth
	  || depth > outer_depth + param_outer_loop_num)
	continue;
      /* Extend outer loop.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\nStart extending outer loop\n");
      /* Superloops of the loop, start from the loop closest to the \
	  current loop in the outermost loop.  */
      for (unsigned j = 0; j < param_outer_loop_num && --depth; ++j)
	{
	  class loop* outer_loop = (*loop->superloops)[depth];
	  /* The outer loop may be added when analyzing previous inner loops,
	     i.e. the outer loop contains two or more inner loops.  */
	  if (loop_refs.count (outer_loop))
	    continue;
	  /* phase1~phase3 analysis on the extended outer loop.  */
	  analyze_loop_dense_memory (kernels, loop_refs, outer_loop);
	  if (loop_refs.count (outer_loop) == 0)
	    continue;
	  for (unsigned k = 0; k < loop_refs[outer_loop].size (); ++k)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "outer_analyze_nested_kernels %d: ", k);
		  print_generic_expr (dump_file, loop_refs[outer_loop][k].ref,\
		  TDF_SLIM);
		  fprintf (dump_file, "\n");
		}
	    }
	  trace_loop_refs_info (loop_refs[outer_loop], traced_ref_stmt);
	  analyze_loop_refs_dimension (loop_refs[outer_loop]);
	  outer_depth = trace_outer_loop_depth (get_cur_loop_niters \
					       (loop_refs, outer_loop), depth);
	  /* `outer_depth == depth` means the current loop is the loop which
	   boundary is known, so there is no need to extend the outer loop.  */
	  if (outer_depth == depth)
	    break;
	  else
	    /* The outer loop cannot find the current loop boundary,
	       Remove the record of outer_loop from the loop_refs.  */
	    loop_refs.erase (outer_loop);
	}
    }
  return true;
}

/* ================ phase 4 filter_and_sort_kernels ================  */

/* Get the edge probability information of each basic block in the loop.  */

float
get_edge_prob (edge e, float minimum)
{
  float fvalue = 0;

  profile_probability probability = e->probability;
  if (probability.initialized_p ())
    {
      fvalue = probability.to_reg_br_prob_base () / float (REG_BR_PROB_BASE);
      if (fvalue < minimum && probability.to_reg_br_prob_base ())
	fvalue = minimum;
    }
  return fvalue;
}

/* Get the next bb with a high branch probability.  */

basic_block
next_high_probability_bb (basic_block bb)
{
  if (bb == NULL)
    return NULL;

  /* Limit the minimum probability value.  */
  const float MINNUM_PROB = 0.00001f;
  float minimum = MINNUM_PROB;

  gimple *stmt = last_stmt (bb);
  if (stmt && gimple_code (stmt) == GIMPLE_COND)
    {
      edge true_edge = NULL;
      edge false_edge = NULL;
      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

      float true_edge_prob = get_edge_prob (true_edge, minimum);
      float false_edge_prob = get_edge_prob (false_edge, minimum);
      /* If the content of the branch does not include the candidate
	 kernel, the branch probability may not be limited.  */
      /* The edge_prob may have precision error during static prediction,
	 so we need to relax the limit before comparison.  */
      if ((true_edge_prob >= (param_branch_prob_threshold / 100.0) - minimum)
	  && flow_bb_inside_loop_p (bb->loop_father, true_edge->dest))
	return true_edge->dest;
      else if ((false_edge_prob >= (param_branch_prob_threshold / 100.0)
		- minimum) && flow_bb_inside_loop_p (bb->loop_father,
		false_edge->dest))
	return false_edge->dest;
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "No high probability bb:");
	      fprintf (dump_file, "current bb: %d, true: %f, false: %f\n",
		       bb->index, true_edge_prob, false_edge_prob);
	    }
	  return NULL;
	}
    }
  else
    {
      edge e = find_fallthru_edge (bb->succs);
      if (e)
	return e->dest;
    }
  return NULL;
}


/* Dump loop header bb.  */

void
dump_loop_headers (const char *name, vector<class loop *> &loops)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
  {
    fprintf (dump_file, "\n\n%s:\n", name);
    fprintf (dump_file, "{ ");
    for (unsigned int i = 0; i < loops.size (); i++)
      fprintf (dump_file, "%d(%d) ", loops[i]->num, loops[i]->header->index);
    fprintf (dump_file, "}\n\n");
  }
}

/* Combine and sort candidate loops.  */

bool
filter_and_sort_kernels (vector<class loop *> &sorted_kernels,
			 vector<class loop *> &kernels)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nPhase 4: filter_and_sort_kernels:\n\n");

  set<basic_block> end_bb;
  list<basic_block> walked_header_bb; /* Used to record nested loops.  */

  for (unsigned i = 0; i < kernels.size (); ++i)
    end_bb.insert (kernels[i]->header);

  dump_loop_headers ("kernels", kernels);

  if (!param_filter_kernels)
    {
      for (vector<class loop *>::iterator it = kernels.begin ();
	   it != kernels.end (); ++it)
	sorted_kernels.push_back (*it);
    }
  else
    {
      basic_block bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

      while (bb)
	{
	  if (bb == NULL)
	    return false;
	  if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
	    break;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "%d ", bb->index);

	  /* bb is not the head of the loop, go to the next.  */
	  if (bb != bb->loop_father->header)
	    {
	      bb = next_high_probability_bb (bb);
	      continue;
	    }

	  /* bb is the head of the loop.  */
	  if (bb != walked_header_bb.back ())
	    {
	      if (end_bb.count (bb))
		{
		  sorted_kernels.push_back (bb->loop_father);
		  bb = single_exit (bb->loop_father)->dest;
		  continue;
		}
	      if (loop_outer (bb->loop_father) != NULL
		  && get_loop_exit_edges (bb->loop_father).length () != 1)
		return false;
	      walked_header_bb.push_back (bb);
	      bb = next_high_probability_bb (bb);
	      continue;
	    }
	  else
	    {
	      walked_header_bb.pop_back ();
	      bb = single_exit (bb->loop_father)->dest;
	      continue;
	    }
	}
    }

  dump_loop_headers ("sorted_kernels", sorted_kernels);
  return true;
}

/* ================ phase 5 record_and_sort_ref_groups ================  */
/* Memory reference score, different aspects of one memory reference.  */

struct ref_score
{
  /* certain memory reference.  */
  data_ref d_ref;

  /* local count for bb where memory reference is located.  */
  gcov_type bb_count;

  /* line-location of memory reference.  */
  int line;
};


/* Memory reference group, different reference of the same variable.  */

struct ref_group
{
  /* source variables.  */
  tree var;

  /* variable size, Unit: MB.  */
  double var_size;

  /* first ref for insert hint.  */
  data_ref first_use;

  /* reuse scores of variables.  */
  unsigned int reuse_level;

  /* method of calculating the var size.  */
  calc_type calc_by;

  /* memory reference index for specific variable.  */
  unsigned int mem_ref_index;

  /* Accessing Reference Records in Different Modes (key_index):
    000: write, random, non-parallel
    001: write, random, parallel
    010: write, regular, non-parallel
    011: write, regular, parallel
    100: read, random, non-parallel
    101: read, random, parallel
    110: read, regular, non-parallel
    111: read, regular, parallel
  */
  map<int, vector<data_ref> > ref_use;

  /* scores for different memory references.  */
  vector<ref_score> ref_scores;

  ref_group ()
    {
      var = NULL_TREE;
      var_size = 0;
      reuse_level = 0;
      calc_by = UNHANDLE_CALC;
      mem_ref_index = 0;
    }
};

/* calculate reuse level.  */

unsigned int
calculate_reuse_level (map<int, vector<data_ref> > &var_use)
{
  unsigned int level = 0;
  for (map<int, vector<data_ref> >::iterator it = var_use.begin ();
       it != var_use.end (); ++it)
    {
      unsigned int parallel = 1;
      unsigned int regular = 1;
      unsigned int cost = 1;

      if ((*it).second[0].parallel_p)
	parallel = PARALLEL_NUM;
      if (!(*it).second[0].regular_p)
	regular = INDIRECT_ACCESS_VALUE;
      if (!(*it).second[0].read_p)
	cost = WRITE_COST;

      /* In serial reuse, we will later check whether they are in the
	 same cacheline.  If yes, delete the reuse.  For details, see the
	 reuse analysis of prefetching and eliminate redundancy.  */
      unsigned int add = parallel * ((*it).second.size () * (cost + regular));
      level += add;
      if (add && dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "%d : %d * (%ld * (%d + %d)) = %d\n",
	      (*it).first, parallel, (*it).second.size (), cost, regular, add);
    }
  return level;
}

/* Comparison of reference reuse level.  */

bool
ref_group_reuse_cmp (const ref_group &a, const ref_group &b)
{
  return a.reuse_level > b.reuse_level;
}

/* Sort reference groups.  */

void
sort_ref_groups (vector<ref_group> &ref_groups,
		 map<tree, ref_group> &ref_groups_map)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nsort_ref_groups_by_reuse_level\n");

  for (map<tree, ref_group>::iterator it = ref_groups_map.begin ();
       it != ref_groups_map.end (); ++it)
    {
      (*it).second.reuse_level = calculate_reuse_level ((*it).second.ref_use);
      ref_groups.push_back ((*it).second);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  print_generic_expr (dump_file, (*it).second.var, TDF_SLIM);
	  fprintf (dump_file, " : %d\n", (*it).second.reuse_level);
	}
    }

  sort (ref_groups.begin (), ref_groups.end (), ref_group_reuse_cmp);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nsorted ref_groups:\n");
      fprintf (dump_file, "rank var (data_size, num_of_mem_ref, need_tmp_name):"
	       " reuse_level_score\n");
      for (unsigned int i = 0; i < ref_groups.size (); ++i)
	{
	  fprintf (dump_file, "%d ", i);
	  print_generic_expr (dump_file, ref_groups[i].var, TDF_SLIM);
	  int need_tmp_name = !get_name (ref_groups[i].var) ? 1 : 0;
	  fprintf (dump_file, " (%lf, %lu, %d)", ref_groups[i].var_size,
		   ref_groups[i].ref_scores.size (), need_tmp_name);
	  fprintf (dump_file, " : %d\n", ref_groups[i].reuse_level);
	}
      fprintf (dump_file, "\n");

      fprintf (dump_file, "first_use:\n");
      for (unsigned int i = 0; i < ref_groups.size (); ++i)
	{
	  fprintf (dump_file, "%d ", i);
	  print_generic_expr (dump_file, ref_groups[i].var, TDF_SLIM);
	  fprintf (dump_file, " : ");
	  if (!ref_groups[i].first_use.vectorize_p)
	    print_generic_expr (dump_file, ref_groups[i].first_use.ref,
				TDF_SLIM);
	  else
	    print_gimple_stmt (dump_file, ref_groups[i].first_use.stmt,
				TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      fprintf (dump_file, "\n");
    }
}

/* Attributes of variable data.  */

enum data_attribute
{
  DA_PARALLEL = 0,
  DA_REGULAR,
  DA_READ
};

/* Record memory reference by use mode.
   If the reference group is not found, create a group.  */

void
record_mem_ref (map<tree, ref_group> &ref_groups, data_ref &mem_ref)
{
  unsigned int index = (mem_ref.parallel_p << DA_PARALLEL)
	      + (mem_ref.regular_p << DA_REGULAR) + (mem_ref.read_p << DA_READ);

  if (!ref_groups.count (mem_ref.var))
    {
      ref_group ref_group;
      ref_group.var = mem_ref.var;
      ref_group.first_use = mem_ref;
      ref_groups[mem_ref.var] = ref_group;
    }

  /* Ref_groups' calc_by depends on the inserted mem_ref's calc_by.
     Runtime issue requires the specified mem_ref's calc_by to be >= 1.
     Temporarily modified ref_group's first_use after sorting mem_refs.  */
  ref_groups[mem_ref.var].calc_by = max (ref_groups[mem_ref.var].calc_by,
					 mem_ref.calc_by);
  ref_groups[mem_ref.var].var_size = max (ref_groups[mem_ref.var].var_size,
					  mem_ref.data_size);
  ref_groups[mem_ref.var].ref_use[index].push_back (mem_ref);

  ref_score ref_level{ mem_ref, ((mem_ref.stmt)->bb->count).to_gcov_type (),
			   expand_location (mem_ref.stmt->location).line };
  ref_groups[mem_ref.var].ref_scores.push_back (ref_level);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "recorded in: ");
      print_generic_expr (dump_file, mem_ref.var, TDF_SLIM);
      fprintf (dump_file, ":%d:%ld\n", index,
	       ref_groups[mem_ref.var].ref_use[index].size () - 1);

      fprintf (dump_file, "base: ");
      print_generic_expr (dump_file, mem_ref.base, TDF_SLIM);

      fprintf (dump_file, ", index: ");
      print_generic_expr (dump_file, mem_ref.index, TDF_SLIM);

      fprintf (dump_file, ", step: ");
      if (mem_ref.step && cst_and_fits_in_hwi (mem_ref.step))
	fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC,
		 int_cst_value (mem_ref.step));
      else
	print_generic_expr (dump_file, mem_ref.step, TDF_SLIM);

      fprintf (dump_file, ", offset: ");
      if (mem_ref.offset && cst_and_fits_in_hwi (mem_ref.offset))
	fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC,
		int_cst_value (mem_ref.offset));
      else
	print_generic_expr (dump_file, mem_ref.offset, TDF_SLIM);
      fprintf (dump_file, ", %s", mem_ref.read_p ? "read" : "write");

      fprintf (dump_file, ", size: %lf", mem_ref.data_size);
      fprintf (dump_file, "\n\n");
    }
}

/* Rank data reference index level by the scheme of source code line number.  */

bool
data_ref_reuse_cmp (const ref_score &a, const ref_score &b)
{
  return a.line < b.line;
}

/* Sort data reference index level within one reference group in non-decreasing
   order of the customized sorting scheme.  */

void
sort_mem_ref_in_ref_group (map<tree, ref_group> &ref_groups_map)
{
  if (dump_file)
    fprintf (dump_file, "\nsorted data_references:\n");
  for (map<tree, ref_group>::iterator it = ref_groups_map.begin ();
       it != ref_groups_map.end (); ++it)
    {
      vector<ref_score> &ref_scores = (*it).second.ref_scores;
      stable_sort (ref_scores.begin (), ref_scores.end (), data_ref_reuse_cmp);
      /* Update ref_group's first_use and calc_by with the first mem_ref after
	 sorting.  */
      (*it).second.first_use = (*it).second.ref_scores[0].d_ref;
      (*it).second.calc_by = (*it).second.first_use.calc_by;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  print_generic_expr (dump_file, (*it).first, TDF_SLIM);
	  fprintf (dump_file, " : %lu\n", ref_scores.size ());
	  for (unsigned int i = 0; i < ref_scores.size (); ++i)
	    {
	      fprintf (dump_file, "mem_ref_index %u: ", i);
	      print_gimple_stmt (dump_file, ref_scores[i].d_ref.stmt, 0,
				 TDF_LINENO);
	    }
	  fprintf (dump_file, "\n\n");
	}
    }
}

/* Tracing and sorting reference groups.  */

bool
record_and_sort_ref_groups (vector<ref_group> &ref_groups,
			    vector<class loop *> &kernels,
			    map<class loop*, vector<data_ref> > &loop_refs)
{
  if (dump_file)
    fprintf (dump_file, "\nPhase 5: trace_all_references_details\n\n");

  map<tree, ref_group> ref_groups_map;

  for (unsigned i = 0; i < kernels.size (); ++i)
    {
      class loop* loop = kernels[i];
      if (loop_refs.count (loop) == 0)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "loop header %d:\n", loop->header->index);
      for (unsigned j = 0; j < loop_refs[loop].size (); ++j)
	{
	  if (loop_refs[loop][j].trace_status_p)
	    record_mem_ref (ref_groups_map, loop_refs[loop][j]);
	}
    }

  /* Sort mem_ref within ref_group by local count and update first_use's
     data_ref, stable sort.  */
  sort_mem_ref_in_ref_group (ref_groups_map);
  sort_ref_groups (ref_groups, ref_groups_map);

  return ref_groups.size () > 0;
}

/* ================ phase 6 issue_llc_hint ================  */

/* Issue vectorized mask prefetch gimple.  */

void
issue_mask_prefetch (gimple *stmt)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "insert svprfd.\n");

  /* vect__1.1 = .MASK_LOAD (_2, 32B, loop_mask_3);
     .MASK_STORE (_4, 32B, loop_mask_5, vect__6.6);
  */
  tree dataref_ptr = gimple_call_arg (stmt, 0);
  tree scale = gimple_call_arg (stmt, 1);
  tree final_mask = gimple_call_arg (stmt, 2);
  tree target = NULL_TREE;
  if (gimple_call_internal_fn (stmt) == IFN_MASK_STORE)
    target = gimple_call_arg (stmt, 3);
  else if (gimple_call_internal_fn (stmt) == IFN_MASK_LOAD)
    target = gimple_call_lhs (stmt);
  /* 4: PLDL3KEEP.  */
  tree prfop = build_int_cst (TREE_TYPE (integer_zero_node), 4);

  /* add offset.  */
  gimple_stmt_iterator si = gsi_for_stmt (stmt);
  /* target: vector_type - XXX_type.  */
  if (target == NULL_TREE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "unhandled scene: target vect is null");
      return;
    }
  HOST_WIDE_INT distance = param_prefetch_offset * tree_to_uhwi
		       (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (target))));
  tree addr = fold_build_pointer_plus_hwi (dataref_ptr, distance);
  addr = force_gimple_operand_gsi (&si, unshare_expr (addr), true,
				    NULL, true, GSI_SAME_STMT);

  gcall *call = gimple_build_call_internal (IFN_MASK_PREFETCH,
				5, addr, scale, final_mask, target, prfop);
  gsi_insert_after (&si, call, GSI_SAME_STMT);
  update_ssa (TODO_update_ssa_only_virtuals);
}

/* Issue vectorized mask gather prefetch gimple.  */

void
issue_mask_gather_prefetch (gimple *stmt)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "insert svprfd_gather_uxindex.\n");

  /* vect_patt_1.1 = .MASK_GATHER_LOAD (_2, vect__3.3, 8, { 0.0, ... },
					loop_mask_4);  */
  tree dataref_ptr = gimple_call_arg (stmt, 0);
  tree vec_offset = gimple_call_arg (stmt, 1);
  tree scale = gimple_call_arg (stmt, 2);
  tree zero = gimple_call_arg (stmt, 3);
  tree final_mask = gimple_call_arg (stmt, 4);
  tree prfop = build_int_cst (TREE_TYPE (integer_zero_node), 4);
  tree target = gimple_call_lhs (stmt);

  /* add offset.  */
  gimple_stmt_iterator si = gsi_for_stmt (stmt);
  if (target == NULL_TREE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "unhandled scene: target vect is null");
      return;
    }
  HOST_WIDE_INT distance = param_prefetch_offset * tree_to_uhwi
		       (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (target))));
  tree addr = fold_build_pointer_plus_hwi (dataref_ptr, distance);
  addr = force_gimple_operand_gsi (&si, unshare_expr (addr), true,
				   NULL, true, GSI_SAME_STMT);

  gcall *call = gimple_build_call_internal
		(IFN_MASK_GATHER_PREFETCH, 7, addr,
		 vec_offset, scale, zero, final_mask, target, prfop);
  gsi_insert_after (&si, call, GSI_SAME_STMT);
  update_ssa (TODO_update_ssa_only_virtuals);
}

/* Issue builtin prefetch gimple.  */

void
issue_builtin_prefetch (data_ref &mem_ref)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "insert prfm.\n");
  /* MEM[symbol: diagPtr, index: ivtmp_102, step: 8, offset: 0B] */
  gimple* stmt = mem_ref.stmt;
  tree dataref_ptr = mem_ref.base;
  tree data_idx = mem_ref.index;
  tree scale = mem_ref.step;
  tree offset = mem_ref.offset;
  /* add offset.  */
  gimple_stmt_iterator si = gsi_for_stmt (stmt);
  if (scale == NULL_TREE)
    {
      /* _190 = (void *) ivtmp.444_221;
	 Cannot detect size unit at (void *).  */
      scale = TYPE_SIZE_UNIT (inner_type (TREE_TYPE (mem_ref.var)));
      if (scale == NULL_TREE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "ERROR: Unknown size unit for the prefetching "
		     "variable.  Stop builtin_prefetch.\n\n");
	  return;
	}
    }

  data_idx = data_idx ? data_idx : size_zero_node;
  data_idx = build1 (NOP_EXPR, TREE_TYPE (scale), data_idx);
  tree displacement = fold_build2 (MULT_EXPR, TREE_TYPE (scale), data_idx,
				   scale);
  if (offset != NULL_TREE && TREE_CODE (offset) != TREE_CODE (size_zero_node))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "WARNING: offset's TREE_TYPE is not integer_cst: "
		 "%s\nStop builtin_prefetch.\n",
		 get_tree_code_name (TREE_CODE (offset)));
      return;
    }
  offset = offset ? offset : size_zero_node;
  offset = build1 (NOP_EXPR, TREE_TYPE (scale), offset);
  dataref_ptr = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (dataref_ptr),
			     dataref_ptr, offset);
  tree addr = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (dataref_ptr),
			   dataref_ptr, displacement);
  HOST_WIDE_INT distance = param_prefetch_offset * tree_to_uhwi (scale);

  addr = fold_build_pointer_plus_hwi (addr, distance);
  addr = force_gimple_operand_gsi (&si, unshare_expr (addr), true,
				   NULL, true, GSI_SAME_STMT);
  /* __builtin_prefetch (_68, 0, 1);
     1st param: *addr, 2nd param: write/read (1/0), 3rd param: temporal locality
     (high means strong locality) */
  gcall *call = gimple_build_call (builtin_decl_explicit (BUILT_IN_PREFETCH),
				3, addr, integer_zero_node, integer_one_node);
  gsi_insert_after (&si, call, GSI_SAME_STMT);
  update_ssa (TODO_update_ssa_only_virtuals);
}

/* Retrieve memory reference at the specific index.  */

data_ref
get_data_ref_at_idx (ref_group &var_ref_group)
{
  unsigned int mem_ref_size = static_cast<unsigned int>(
      			var_ref_group.ref_scores.size ());
  if (strlen (param_mem_ref_index) == 0)
    return var_ref_group.first_use;
  else
    {
      /* Insert prefetch hint at highly-likely-used location with the given
	 index.  */
      if (var_ref_group.mem_ref_index >= mem_ref_size)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "WARNING: The target data_ref index is out "
		     "of range.  Use top index instead!\n");
	  return var_ref_group.ref_scores[0].d_ref;
	}
      return var_ref_group.ref_scores[var_ref_group.mem_ref_index].d_ref;
    }
}

/* Static form insertion and issue instruction.  We may check the
   determination of the ARM SVE architecture before SVE hint insertion.  */

void
static_issue (vector<ref_group> &ref_groups, int num_issue_var)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "static issue\n");

  for (int i = 0; i < num_issue_var; ++i)
    {
      data_ref mem_ref = get_data_ref_at_idx (ref_groups[i]);
      if (mem_ref.vectorize_p)
	{
	  enum internal_fn ifn_code = gimple_call_internal_fn
					(mem_ref.stmt);
	  if (ifn_code == IFN_MASK_STORE || ifn_code == IFN_MASK_LOAD)
	    issue_mask_prefetch (mem_ref.stmt);
	  else if (ifn_code == IFN_MASK_GATHER_LOAD)
	    issue_mask_gather_prefetch (mem_ref.stmt);
	  else
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "other vectorized internal function\n");
	}
      else
	issue_builtin_prefetch (mem_ref);
    }
}

/* Generate the stmts for calculating the size.  Later we will consider nested
   multi-branches scenarios and check more information of niters when it is
   a COND_EXPR.  */

tree
calc_stmts_gen (vector<ref_group> &ref_groups, gimple_seq &cond_expr_stmt_list,
		int num_issue_var)
{
  /* Accumulated keep size.  */
  tree total_size = build_real_from_int_cst
		      (double_type_node, integer_zero_node);
  for (int i = 0; i < num_issue_var; ++i)
    {
      data_ref &mem_ref = ref_groups[i].first_use;
      tree var = mem_ref.var;
      for (unsigned j = 0; j < mem_ref.loop_bounds.size (); ++j)
	{
	  tree niters = mem_ref.loop_bounds[j].niters;

	  /* COND_EXPR.  */
	  if (TREE_CODE (niters) == COND_EXPR)
	    niters = TREE_OPERAND (niters, 1);
	  tree unit = TYPE_SIZE_UNIT (inner_type (TREE_TYPE (var)));
	  /* _190 = (void *) ivtmp.444_221;
	     Cannot detect size unit at (void *).  */
	  if (unit == NULL_TREE)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "WARNING: Cannot detect size unit "
			   "(use 1 byte) for variable %s: ", get_name (var));
		  print_generic_expr (dump_file, mem_ref.ref, TDF_SLIM);
		  fprintf (dump_file, "\n");
		}
	      unit = size_one_node;
	    }
	  unit = build1 (NOP_EXPR, TREE_TYPE (niters), unit);
	  tree size = fold_build2 (MULT_EXPR, TREE_TYPE (niters), niters, unit);
	  size = build1 (FLOAT_EXPR, double_type_node, size);
	  total_size = fold_build2
			 (PLUS_EXPR, double_type_node, total_size, size);
	}
    }
  /* Create a stmt list for size calculation.  */
  tree div = build_int_cst (TREE_TYPE (integer_zero_node), 1024 * 1024);
  div = build1 (NOP_EXPR, double_type_node, div);
  total_size = fold_build2 (RDIV_EXPR, double_type_node, total_size, div);

  tree threshold = build_int_cst (TREE_TYPE (integer_zero_node),
				  param_llc_capacity_per_core / 2);
  threshold = build_real_from_int_cst (double_type_node, threshold);
  tree cond_expr = fold_build2
		     (LE_EXPR, boolean_type_node, total_size, threshold);

  /* Convert cond_expr to stmt list.  */
  cond_expr = force_gimple_operand_1 (unshare_expr (cond_expr),
	      &cond_expr_stmt_list, is_gimple_condexpr, NULL_TREE);
  return cond_expr;
}

/* Runtime form insertion and issue instruction.  */

void
runtime_issue (vector<ref_group> &ref_groups, int num_issue_var)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "runtime issue\n");

  if (ref_groups.size () == 0)
    return;
  data_ref &mem_ref = ref_groups[0].first_use;
  class loop *loop = mem_ref.loop_bounds.back ().loop;
  /* Ensure that variables are in the same loop.  */
  for (int i = 1; i < num_issue_var; ++i)
    {
      data_ref &mem_ref = ref_groups[i].first_use;
      if (loop != mem_ref.loop_bounds.back ().loop)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "topn var are not in the same loop\n");
	  return;
	}
    }
  if (loop == NULL)
    return;

  /* If the exit edge points to bb with multiple inputs, split the exit edge
     and create a new bb, make the exit edge point to bb only single input.  */
  edge e = single_exit (loop);
  if (e == NULL)
    return;
  if (!single_pred_p (e->dest))
    {
      split_loop_exit_edge (e, true);
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "split exit edge\n");
    }

  gimple_seq cond_expr_stmt_list = NULL;
  tree cond_expr = calc_stmts_gen (ref_groups, cond_expr_stmt_list,
				   num_issue_var);

  /* Use the previous cond and generate a new branch and copy loop.  */
  basic_block condition_bb = NULL;
  profile_probability prob = profile_probability::likely ();
  initialize_original_copy_tables ();
  class loop *nloop = loop_version (loop, cond_expr, &condition_bb,
		      prob, prob.invert (), prob, prob.invert (), true);
  free_original_copy_tables ();

  /* Insert the generated stmt list before cond_expr.  */
  gimple_stmt_iterator cond_exp_gsi;
  if (cond_expr_stmt_list)
    {
      cond_exp_gsi = gsi_last_bb (condition_bb);
      gsi_insert_seq_before (&cond_exp_gsi, cond_expr_stmt_list,
	  GSI_SAME_STMT);
    }
  update_ssa (TODO_update_ssa);

  /* Perform hint issue for branches that meet conditions.  */
  static_issue (ref_groups, num_issue_var);
}

/* Issue llc hints through prefetch instructions.  */

void
issue_llc_hint (vector<ref_group> &ref_groups)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "issue_llc_hint:\n");

  /* 1. If the issue-topn and force-issue options are available, top N var is
	forcibly allocated and no runtime branch is generated.
     2. If the issue-topn option is available and the size of top N var is
	statically known, top N is statically allocated and no runtime branch
	is generated.
     3. If the issue-topn option is available and the size of the top N var is
	unknown, but them is dynamically known, the top N is dynamically
	allocated and generate runtime branches. (also depends on the screening
	of the innermost variable boundary type)
     4. If the dynamic runtime cannot know the size, such as indirect access,
	optimization is skipped.
  */
  if (ref_groups.size () == 0)
    return;

  int num_issue_var = min (param_issue_topn,
			   static_cast<int>(ref_groups.size ()));
  if (num_issue_var < param_issue_topn
      && dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "WARNING: Only %u (less than param_issue_topn = %d) "
	       "ref_group(s) is found for llc hint.\n",
	       num_issue_var, param_issue_topn);
    }
  if (param_force_issue)
    {
      if (strlen (param_target_variables) > 0)
	static_issue (ref_groups, static_cast<int>(ref_groups.size ()));
      else
	static_issue (ref_groups, num_issue_var);
      return;
    }
  calc_type topn_calc_type = STATIC_CALC;
  for (int i = 0; i < num_issue_var; ++i)
    topn_calc_type = min (topn_calc_type, ref_groups[i].calc_by);

  if (topn_calc_type == STATIC_CALC)
    {
      /* Before static issue, we still need to collect data size of all target
	 variables and compare the summation with LLC cache size.  */
      double prefetch_data_size = 0.;
      for (int i = 0; i < num_issue_var; ++i)
	prefetch_data_size += ref_groups[i].var_size;
      if (prefetch_data_size <= (double) param_llc_capacity_per_core * 0.8)
	static_issue (ref_groups, num_issue_var);
      else
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "static issue: Prefetch size exceeds LLC cache "
		   "size: %lf > %lf.\n", prefetch_data_size,
		   (double) param_llc_capacity_per_core * 0.8);
    }
  else if (topn_calc_type == RUNTIME_CALC)
    runtime_issue (ref_groups, num_issue_var);
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "unhandled issue scene\n");
    }
}

/* ==================== phase entry ====================  */
/* Check whether a string can be converted to an unsigned integer.  */

bool is_unsigned_int (const string &s)
{
  if (s.empty () || s.size () > PREFETCH_TOOL_NUM_MAX_LEN)
    return false;

  for (unsigned int i = 0; i < s.size (); ++i)
    {
      if (s[i] < '0' || s[i] > '9')
	return false;
    }
  return true;
}

/* Parse a substring separated by comma.  If the substring is valid and
   non-empty, store it as a parsed element.  */

bool
parse_string_helper (const string &substr, vector<string>& str_elts,
		     bool check_unsigned, size_t start, size_t end)
{
  if (substr == "" && dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "WARNING: The input string from %lu to %lu is "
	     "empty.\n", start, end);
  else if (check_unsigned && !is_unsigned_int (substr))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "ERROR: not an unsigned integer: %s\n",
		 substr.c_str ());
      str_elts.clear ();
      return false;
    }
  else
    str_elts.push_back (substr);
  return true;
}

/* Parse a user input string, separated by comma.  */

void
parse_string (const string &s, vector<string>& str_elts,
	      bool check_unsigned = false)
{
  string delim = ",";
  size_t start = 0;
  size_t end = s.find (delim);
  string substr = s.substr (start, end - start);
  while (end != string::npos)
    {
      if (!parse_string_helper (substr, str_elts, check_unsigned, start, end))
	return;
      start = end + delim.size ();
      end = s.find (delim, start);
      substr = s.substr (start, end - start);
    }
  parse_string_helper (substr, str_elts, check_unsigned, start, end);
}

/* Parse user input of target variables and memory indices and create a map
   that assigns a target variable to a memory index.  */

void
parse_param_inputs (map<string, unsigned int> &var2mem_idx)
{
  /* The user input length should have an input length limit.  */
  if ((strlen (param_target_variables) >= PREFETCH_TOOL_INPUT_MAX_LEN
       || strlen (param_mem_ref_index) >= PREFETCH_TOOL_INPUT_MAX_LEN)
      && dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "INVALID INPUT: The user inputs for target variables "
	      "and/or memory reference indices are too long for parsing.\n");

  vector<string> var_names;
  string target_variables = param_target_variables;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Start parsing target variables:\n");
  if (param_use_ref_group_index)
    parse_string (target_variables, var_names, true);
  else
    parse_string (target_variables, var_names, false);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Finish parsing target variables.\n\n");

  vector<string> var_mem_indices;
  string mem_indices = param_mem_ref_index;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Start parsing memory reference indices:\n");
  parse_string (mem_indices, var_mem_indices, true);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Finish parsing memory reference indices.\n\n");

  /* Construct a map of var_name: var_mem_index.  */
  if (var_names.size () > 0)
    {
      if (var_mem_indices.size () < var_names.size ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "WARNING: The number of provided memory "
		     "reference indices is less than that of target "
		     "variables.\nUse the top index for all variables "
		     "instead.\n");
	  for (string& var_name : var_names)
	    var2mem_idx[var_name] = 0;
	}
      else
	{
	  if (var_mem_indices.size () > var_names.size ()
	      && dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "WARNING: The number of target variables is "
		     "less than that of memory reference indices.\n");
	  for (unsigned int i = 0; i < var_names.size (); ++i)
	    {
	      var2mem_idx[var_names[i]] = static_cast<unsigned int>(
		atoi (var_mem_indices[i].c_str ()));
	    }
	}
    }
}

/* Filter reference groups by only selecting target variables from the user
   input.  There are two options for prefetching target variables:
   1. Specify variable name parsed by the pass, which you can double-check at
      "sorted ref_groups" section in the dump file.
   2. Specify variable rank exhibited at "sorted ref_groups" section in the
      dump file.
*/

void
prefetch_variables (const vector<ref_group>& ref_groups,
		    vector<ref_group>& reduced_ref_groups)
{
  map<unsigned int, unsigned int> ref_group2mem_idx;

  map<string, unsigned int> var2mem_idx;  /* externally defined.  */
  parse_param_inputs (var2mem_idx);

  if (param_use_ref_group_index)
    {
      /* Use ref_group index at "sorted ref_groups" section to specify
	 variable.  */
      /* Collect the variables in "reduced_ref_group" only if their indices
	show up at "sorted ref_groups" section.  */
      for (const pair<string, unsigned int> &var_mem_idx : var2mem_idx)
	{
	  unsigned int var_idx = static_cast<unsigned int>(atoi (
				    var_mem_idx.first.c_str ()));
	  if (var_idx < ref_groups.size ())
	    ref_group2mem_idx[var_idx] = var_mem_idx.second;
	  else if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "WARNING: The index \"%u\" does not show "
		      "up in the ref_groups.\n", var_idx);
	}
    }
  else
    {
      /* Use variable name shown up at "sorted ref_groups" section to specify
	 variable:
	 var2ref_group_idx + var2mem_idx -> ref_group2mem_idx.  */
      /* Create a map that assigns the variable name to its corresponding
	 ref_group index.  */
      map<string, unsigned int> var2ref_group_idx;  /* internally detected.  */
      for (unsigned int i = 0; i < ref_groups.size (); ++i)
	{
	  const ref_group &curr_ref_group = ref_groups[i];
	  const int UINT_MAX_DIGIT = 10;
	  /* Unrecognizable variable name related to ref_group.  */
	  if (!get_name (curr_ref_group.var))
	    {
	      /* If the variable name does not have a string representation,
		 we can rename it by "tmp_var_" + <sorted_ref_group_index>.  */
	      char group_idx[UINT_MAX_DIGIT];
	      sprintf (group_idx, "%u", i);
	      string tmp_var_name = "tmp_var_" + std::string (group_idx);
	      fprintf (dump_file, "Unrecognizable variable name at ref_group "
		       "index %u.\nThe tree expression for variable is: ", i);
	      print_generic_expr (dump_file, curr_ref_group.var, TDF_SLIM);
	      fprintf (dump_file, "\n");
	      var2ref_group_idx[tmp_var_name] = i;
	    }
	  else
	    var2ref_group_idx[std::string (get_name (curr_ref_group.var))] = i;
	}
      /* Collect the variables in "reduced_ref_group" only if they show up in
	 the ref_groups.  */
      for (const pair<string, unsigned int> &var_mem_idx : var2mem_idx)
	{
	  if (var2ref_group_idx.count (var_mem_idx.first))
	    {
	      unsigned int ref_group_idx = var2ref_group_idx[var_mem_idx.first];
	      ref_group2mem_idx[ref_group_idx] = var_mem_idx.second;
	    }
	  else if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "WARNING: Target variable \" %s \" does "
		      "not show up in the ref_groups.  Check whether it needs "
		      "temporary variable name.\n",
		      var_mem_idx.first.c_str ());
	}
    }

  for (const pair<unsigned int, unsigned int> &ref_group_mem_idx :
       ref_group2mem_idx)
    {
      ref_group curr_ref_group = ref_groups[ref_group_mem_idx.first];
      curr_ref_group.mem_ref_index = ref_group_mem_idx.second;
      reduced_ref_groups.push_back (curr_ref_group);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nNOTICE: Prefetching target variable \" ");
	  print_generic_expr (dump_file, curr_ref_group.var, TDF_SLIM);
	  fprintf (dump_file, " \" at ref_group index %u and memory location "
		   "index %u.\n", ref_group_mem_idx.first,
		   ref_group_mem_idx.second);
	}
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\n");
}


/* The LLC intelligent allocation consists of 6 steps.  */

void
llc_allocate (void)
{
  map<class loop *, vector<data_ref> > kernels_refs;
  vector<class loop *> kernels;
  if (!get_dense_memory_kernels (kernels, kernels_refs))
    return;

  set <gimple *> traced_ref_stmt;
  trace_data_refs_info (kernels, kernels_refs, traced_ref_stmt);

  if (!analyze_nested_kernels (kernels, kernels_refs, traced_ref_stmt))
    return;

  vector<class loop *> sorted_kernels;
  if (!filter_and_sort_kernels (sorted_kernels, kernels))
    return;

  vector<ref_group> ref_groups;
  if (!record_and_sort_ref_groups (ref_groups, sorted_kernels, kernels_refs))
    return;

  if (strlen (param_target_variables) > 0)
    {
      /* If "param_target_variables" is not empty, we will issue parsed target
	variables compulsorily.  */
      param_force_issue = true;
      vector<ref_group> reduced_ref_groups;
      prefetch_variables (ref_groups, reduced_ref_groups);
      issue_llc_hint (reduced_ref_groups);
    }
  else
    issue_llc_hint (ref_groups);
}

/* Check whether the function is an operator reloading function.  */

bool
operator_func_p (function *fn)
{
  const char *fn_name = IDENTIFIER_POINTER (DECL_NAME (fn->decl));

  if (fn_name && strncmp (fn_name, "operator", 8) == 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "operator_func: %s ", fn_name);

      return true;
    }
  return false;
}

/* Check whether the function file location is known.  */

bool
func_location_p (function *fn)
{
  expanded_location fn_decl_xloc
    = expand_location (DECL_SOURCE_LOCATION (current_function_decl));
  expanded_location fn_xloc
    = expand_location (fn->function_start_locus);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "fn->function_start_locus = %d \n",
	       fn->function_start_locus);
      fprintf (dump_file, "fn_xloc.file = %s \n",
	       fn_xloc.file ? fn_xloc.file : "NULL");
      fprintf (dump_file, "fn_decl_xloc.file = %s \n",
	       fn_decl_xloc.file ? fn_decl_xloc.file : "NULL");
      fprintf (dump_file, "LOCATION_FILE (input_location) = %s \n",
	LOCATION_FILE (input_location) ? LOCATION_FILE (input_location)
				       : "NULL");
    }
  if (fn_decl_xloc.file == NULL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Function location unknown, skip analysis \n");
      return false;
    }
  /* Newly generated functions are filtered out, such as function constant
     propagation func.constprop ().  */
  if (LOCATION_FILE (input_location) != fn_decl_xloc.file)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Function location non-local, skip analysis \n");
      return false;
    }
  return true;
}

/* Dump function information.  */

void
dump_function_info (function *fn)
{
  const char *fn_name = IDENTIFIER_POINTER (DECL_NAME (fn->decl));
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nfn_name: %s\n", fn_name);
      expanded_location cfun_xloc
	= expand_location (DECL_SOURCE_LOCATION (current_function_decl));
      if (cfun_xloc.line)
	{
	  if (cfun_xloc.file)
	    fprintf (dump_file, "[%s:%d:%d]\n",
		     cfun_xloc.file, cfun_xloc.line, cfun_xloc.column);
	}
      fprintf (dump_file, "\n");
      flow_loops_dump (dump_file, NULL, 1);
      fprintf (dump_file, "\n");
    }
}

/* dump param.  */

void
dump_param (void)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
  {
    fprintf (dump_file, "LLC allocate parameters:\n");
    fprintf (dump_file, "    block size: %d\n", param_l1_cache_line_size);
    fprintf (dump_file, "    L1 cache size: %d lines, %d kB\n",
	param_l1_cache_size * 1024 / param_l1_cache_line_size,
	param_l1_cache_size);
    fprintf (dump_file, "    L1 cache line size: %d\n",
	param_l1_cache_line_size);
    fprintf (dump_file, "    L2 cache size: %d kB\n", param_l2_cache_size);
    fprintf (dump_file, "    min mem_access_ratio: %d \n",
	param_mem_access_ratio);
    fprintf (dump_file, "    min mem_access_num: %d \n",
	param_mem_access_num);
    fprintf (dump_file, "\n");
  }
}

const pass_data pass_data_llc_allocate =
{
  GIMPLE_PASS, /* type.  */
  "llc_allocate", /* name.  */
  OPTGROUP_LOOP, /* optinfo_flags.  */
  TV_TREE_PREFETCH, /* tv_id.  */
  (PROP_cfg | PROP_ssa), /* properties_required.  */
  0, /* properties_provided.  */
  0, /* properties_destroyed.  */
  0, /* todo_flags_start.  */
  0, /* todo_flags_finish.  */
};

class pass_llc_allocate : public gimple_opt_pass
{
public:
  pass_llc_allocate (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_llc_allocate, ctxt)
  {}

  /* opt_pass methods.  */
  virtual bool gate (function *)
    {
      return (optimize >= 2 && flag_llc_allocate > 0);
    }
  virtual unsigned int execute (function *);

}; // class pass_llc_allocate

unsigned int
pass_llc_allocate::execute (function *fn)
{
  unsigned int ret = 0;

  if (!targetm.have_prefetch ()
      || targetm.vectorize.code_for_prefetch == NULL
      || targetm.vectorize.prefetch_handleable_mode_p == NULL
      || targetm.vectorize.code_for_gather_prefetch == NULL)
    return 0;

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

  dump_param ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "llc_allocate: %s\n",
	     IDENTIFIER_POINTER (DECL_NAME (fn->decl)));

  if (number_of_loops (fn) <= 1  || !func_location_p (fn)
      || operator_func_p (fn))
    return ret;

  dump_function_info (fn);

  llc_allocate ();

  return ret;
}

} // anon namespace

gimple_opt_pass *
make_pass_llc_allocate (gcc::context *ctxt)
{
  return new pass_llc_allocate (ctxt);
}
