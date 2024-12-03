/* If-split.
   Copyright (C) 2024 Free Software Foundation, Inc.

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
#define INCLUDE_FUNCTIONAL
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-ssa.h"
#include "tree-pass.h"
#include "diagnostic-core.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "cfg.h"
#include "cfghooks.h"
#include "ssa.h"
#include "fold-const.h"
#include "tree-into-ssa.h"
#include "tree-cfg.h"
#include "bitmap.h"
#include "cfganal.h"
#include "cfgloop.h"

/* Perform splitting if-then-else patterns, whose complex OR condition in
cond-bb contains comparison of some variable with constant and then-bb got
function call, whose arg list contains this var (or this variable is a
scalar of an aggregate which is an arg of this call). We split condition on
two separate ones and duplicate then-bb for each one, thus help ipa const
prop to propagate corresponding constant in function calls.
Example:
	Before:
		if (n == const || some_cond)
			func (n);
	After:
		if (n == const)
			func (n);
		else if (some_cond)
			func (n);  */

//-------------------------------------------------------------------------
// Auxiliary functions
//-------------------------------------------------------------------------
/* Check if arg list of call got n.  */
bool
got_in_args_p (gimple* call, tree n)
{
  unsigned num_args = gimple_call_num_args (call);

  for (int i = 0; i < num_args; i++)
    {
      if (n == gimple_call_arg (call, i))
  return true;
    }

  return false;
}

#define SCALAR_NESTING 2
/* Check if call is "necessary" for n.  Call is called "necessary"
 * for n, if n is one of call args, or n is scalar of some aggregate,
 * which is one of this call args.  Nesting param determines how many
 * levels of aggregate-scalar nesting we want to check.  For example,
 * if nesting == 2, we allow only 2 levels of nesting, like
 * outer_aggr->inner_aggr->scalar.  */
static bool
necessary_call_p (gimple *call, tree n, unsigned nesting)
{
  if (!call)
    return false;

  if (got_in_args_p (call, n))
    return true;

  /* Else we need to check if n could be a scalar of some aggregate which
   * is one of call args.  */
  tree scalar = n;
  tree aggregate = NULL_TREE;

  for (int i = 0; i < nesting; i++)
    {
      if (!scalar || TREE_CODE (scalar) != SSA_NAME)
	return false;

      gimple *scalar_def = SSA_NAME_DEF_STMT (scalar);

      if (!is_gimple_assign (scalar_def)
	  || gimple_assign_rhs_code (scalar_def) != COMPONENT_REF)
	return false;

      tree scalar_def_rhs = gimple_assign_rhs1 (scalar_def);
      tree aggregate = TREE_OPERAND (scalar_def_rhs, 0);

      if (TREE_CODE (aggregate) == MEM_REF)
	aggregate = TREE_OPERAND (aggregate, 0);

      if (aggregate && got_in_args_p (call, aggregate))
	return true;

      scalar = aggregate;
    }

  return false;
}

/* Check if bb got a "necessary" call statement.  */
static bool
bb_got_necessary_call_p (basic_block bb, tree n, unsigned nesting)
{
  gimple *stmt = NULL;

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (is_gimple_call (stmt) && necessary_call_p (stmt, n, nesting))
	return true;
    }

  return false;
}

//-------------------------------------------------------------------------
// Complex conditions
//-------------------------------------------------------------------------
/* Auxiliary struct which contains var and its constant of comaprison
 * of expr: n == cst.  */
struct var_const
{
  tree n = NULL_TREE;
  tree cst = NULL_TREE;
};

/* Check if var_def stmt got this pattern:
 *    var = (n == const);
 * If it does, we need to set var_cst struct.  */
static bool
comp_with_const_p (gimple *var_def, var_const *var_cst)
{
  if (gimple_expr_code (var_def) != EQ_EXPR)
    return false;

  tree var_def_rhs2 = gimple_assign_rhs2 (var_def);

  if (TREE_CODE (var_def_rhs2) != INTEGER_CST)
    return false;

  var_cst->n = gimple_assign_rhs1 (var_def);
  var_cst->cst = var_def_rhs2;

  return true;
}

/* Auxiliary struct which contains defenition of each part of
 * complex condition, like:
 *    a = ... <- a_def
 *    b = ... <- b_def
 *    c = a | b  <- complex_cond.  */
struct cond_parts_defs
{
  gimple *a_def = NULL;
  gimple *b_def = NULL;
};

/* Check if cond got this pattern:
 *    a = ...; <- a_def
 *    b = ...; <- b_def
 *    c = a | b;
 *    if (c != 0)
 * and a_def or b_def is comparison with constant.  If it does,
 * we need to set a with a_def and b with b_def.  */
static bool
necessary_complex_cond_p (const gimple *cond, basic_block then_bb,
			  cond_parts_defs *defs)
{
  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);

  /* As we look for: if (c != 0).  */
  if (gimple_cond_code (cond) != NE_EXPR || TREE_CODE (lhs) != SSA_NAME
      || !integer_zerop (rhs))
    return false;

  gimple *c_def = SSA_NAME_DEF_STMT (lhs);

  /* As we look for: c = a | b.  */
  if (!c_def || !is_gimple_assign (c_def) || gimple_num_ops (c_def) != 3
      || gimple_expr_code (c_def) != BIT_IOR_EXPR)
    return false;

  tree a_var = gimple_assign_rhs1 (c_def);
  tree b_var = gimple_assign_rhs2 (c_def);
  gimple *a_def = SSA_NAME_DEF_STMT (a_var);
  gimple *b_def = SSA_NAME_DEF_STMT (b_var);

  if (!a_def || !is_gimple_assign (a_def) || !b_def
      || !is_gimple_assign (b_def))
    return false;

  var_const var_cst;

  if (!(comp_with_const_p (a_def, &var_cst)
	&& bb_got_necessary_call_p (then_bb, var_cst.n, SCALAR_NESTING))
      && !(comp_with_const_p (b_def, &var_cst)
	   && bb_got_necessary_call_p (then_bb, var_cst.n, SCALAR_NESTING)))
    return false;

  defs->a_def = a_def;
  defs->b_def = b_def;

  return true;
}

/* Check if our complex condition seems to be "necessary"
 * and if it does split it on two separate ones.  Like:
 *    a = (n == const); <- a_def
 *    b = smth; <- b_def
 *    c = a | b
 *    if (c != 0)
 *       call func (n, ...)
 * Transform this to:
 *    if (n == const)
 *	 goto then
 *    else if (b != 0)
 *	 goto then
 *     then:
 *	 call func (n, ...).
 * A complex condition is called "necessary", if it is OR of two
 * conditions, one of them is comparison with constant and then_bb
 * of this cond got "necessary" function_call.  To know, what
 * "necessary" function call means look at necessary_call_p ().  */
static void
process_complex_cond (basic_block cond_bb, basic_block then_bb,
		      basic_block else_bb)
{
  gimple *cond = last_stmt (cond_bb);
  cond_parts_defs defs;

  if (!can_duplicate_block_p (then_bb)
      || !single_succ_p (then_bb)
      || !necessary_complex_cond_p (cond, then_bb, &defs))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Recognized necessary complex condition: ", cond_bb->index);
      print_gimple_stmt (dump_file, cond, 0, TDF_NONE);
    }

  var_const var_cst;

  /* Setting cond.  */
  if (comp_with_const_p (defs.a_def, &var_cst))
      /* Setting cond as: if (n == const).  */
      gimple_cond_set_condition (as_a<gcond *> (cond), EQ_EXPR, var_cst.n,
					var_cst.cst);
  else
    {
      /* Setting cond as: if (a != 0).  */
      tree cond_lhs = gimple_assign_lhs (defs.a_def);
      gimple_cond_set_condition (as_a<gcond *> (cond), NE_EXPR, cond_lhs,
      					build_zero_cst (TREE_TYPE (cond_lhs)));
    }
  update_stmt (cond);

  /* Creating inner_cond_bb.  */
  edge then_e = find_edge (cond_bb, then_bb);
  edge else_e = find_edge (cond_bb, else_bb);
  basic_block inner_cond_bb = split_edge (else_e);

  /* Setting inner_cond.  */
  gcond *inner_cond = NULL;
  if (comp_with_const_p (defs.b_def, &var_cst))
    {
      /* Setting inner cond as: if (b == const).  */
      inner_cond = gimple_build_cond (EQ_EXPR, var_cst.n, var_cst.cst,
				      NULL_TREE, NULL_TREE);
    }
  else
    {
      /* Setting inner cond as: if (b != 0).  */
      tree inner_cond_lhs = gimple_assign_lhs (defs.b_def);
      inner_cond = gimple_build_cond (
	  NE_EXPR, inner_cond_lhs, build_zero_cst (TREE_TYPE (inner_cond_lhs)),
	  NULL_TREE, NULL_TREE);
    }
  gimple_stmt_iterator gsi = gsi_last_bb (inner_cond_bb);
  gsi_insert_after (&gsi, inner_cond, GSI_NEW_STMT);

  /* Configuring edges.  */
  edge inner_cond_then_e = make_edge (inner_cond_bb, then_bb, EDGE_TRUE_VALUE);
  edge inner_cond_else_e = find_edge (inner_cond_bb, else_bb);
  inner_cond_else_e->flags = EDGE_FALSE_VALUE;

  /* Setting phinode args in then_bb coming from inner_cond_bb the same as
   * ones coming from cond_bb.  */
  for (gphi_iterator psi = gsi_start_phis (then_bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      add_phi_arg (phi, PHI_ARG_DEF_FROM_EDGE (phi, then_e), inner_cond_then_e,
		   UNKNOWN_LOCATION);
    }

  /* Updating dominators.  */
  set_immediate_dominator (CDI_DOMINATORS, inner_cond_bb, cond_bb);
  basic_block cond_bb_postdominator
      = get_immediate_dominator (CDI_POST_DOMINATORS, cond_bb);
  set_immediate_dominator (CDI_POST_DOMINATORS, inner_cond_bb,
			   cond_bb_postdominator);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Successfully transformed:\n (o_cond) ",
	       cond_bb->index);
      print_gimple_stmt (dump_file, cond, 0, TDF_NONE);
      fprintf (dump_file, " (i_cond) ", inner_cond_bb->index);
      print_gimple_stmt (dump_file, inner_cond, 0, TDF_NONE);
    }
}

//-------------------------------------------------------------------------
// Condition pairs
//-------------------------------------------------------------------------
/* Transforming cfg if we recognized patern in process_condition_pair (). */
static basic_block
make_two_separate_calls (basic_block outer_cond_bb, basic_block inner_cond_bb,
			 basic_block then_bb)
{
  if (!can_duplicate_block_p (then_bb) || !single_succ_p (then_bb))
    return NULL;

  edge outer_then_e = find_edge (outer_cond_bb, then_bb);

  /* Making duplication of then_bb.  */
  basic_block then_bb_dom = get_immediate_dominator (CDI_DOMINATORS, then_bb);

  /* Saving ret_value and then_bb succ edge flags, if then_bb is pred of
   * EXIT_BLOCK and has return statement inside.  */
  tree ret_val;
  int then_bb_succ_edge_flags;
  if (single_succ (then_bb) == EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      gcc_assert (gimple_code (last_stmt (then_bb)) == GIMPLE_RETURN);
      ret_val = gimple_return_retval (as_a<greturn*>(last_stmt (then_bb)));

      then_bb_succ_edge_flags = single_succ_edge (then_bb)->flags;
    }

  basic_block merge_bb = split_edge (single_succ_edge (then_bb));

  /* Building return statement in merge_bb and setting merge_bb succ edge flags,
   * if now merge_bb is pred of EXIT_BLOCK.  */
  if (single_succ (merge_bb) == EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      gimple* ret = gimple_build_return (ret_val);
      gimple_stmt_iterator gsi = gsi_last_bb (merge_bb);
      gsi_insert_after (&gsi, ret, GSI_NEW_STMT);

      single_succ_edge (merge_bb)->flags = then_bb_succ_edge_flags;
    }

  basic_block then_bb1 = duplicate_block (then_bb, outer_then_e, outer_cond_bb);
  edge outer_then1_e = find_edge (outer_cond_bb, then_bb1);

  /* Setting phinode args in then_bb1 coming from outer_cond_bb by previously
   * collected args_from_outer_cond_bb.  */
  flush_pending_stmts (outer_then1_e);

  /* Updating dominators.  */
  if (then_bb_dom == outer_cond_bb)
    set_immediate_dominator (CDI_DOMINATORS, then_bb, inner_cond_bb);

  set_immediate_dominator (CDI_DOMINATORS, merge_bb, then_bb_dom);
  set_immediate_dominator (CDI_DOMINATORS, then_bb1, outer_cond_bb);

  set_immediate_dominator (CDI_POST_DOMINATORS, then_bb, merge_bb);
  set_immediate_dominator (CDI_POST_DOMINATORS, then_bb1, merge_bb);
  set_immediate_dominator (CDI_POST_DOMINATORS, merge_bb,
			   single_succ (merge_bb));

  if (get_immediate_dominator (CDI_POST_DOMINATORS, outer_cond_bb) == then_bb)
     set_immediate_dominator (CDI_POST_DOMINATORS, outer_cond_bb, merge_bb);

  return then_bb1;
}

/* Here we check if cond of bb got this pattern:
 *    if (n == const)
 * And if it does we need to set n.  */
static bool
got_necessary_cond_p (basic_block bb, tree *n)
{
  gimple *stmt = last_stmt (bb);
  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return false;

  gcond *cond = as_a<gcond *> (stmt);

  if (gimple_cond_code (cond) != EQ_EXPR
      || TREE_CODE (gimple_cond_lhs (cond)) != SSA_NAME
      || TREE_CODE (gimple_cond_rhs (cond)) != INTEGER_CST)
    return false;

  *n = gimple_cond_lhs (cond);

  return true;
}

/* Recognize pattern:
 *    if (n == const)
 *	 goto then
 *    else if (some_cond)
 *	 goto then
 *    then:
 *	 call func (n, ...)
 * Transform this to:
 *    if (n == const)
 *	 call func (n, ...)
 *    else if (some_cond)
 *	 call func (n, ...).  */
static void
process_cond_pair (basic_block outer_cond_bb, basic_block inner_cond_bb,
		   basic_block then_bb)
{
  tree n = NULL_TREE;

  if (inner_cond_bb == then_bb
      || !recognize_if_then_else (outer_cond_bb, &then_bb, &inner_cond_bb)
      || !same_phi_args_p (outer_cond_bb, inner_cond_bb, then_bb)
      || (!got_necessary_cond_p (outer_cond_bb, &n)
	  && !got_necessary_cond_p (inner_cond_bb, &n))
      || !bb_got_necessary_call_p (then_bb, n, SCALAR_NESTING))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Recognized necessary condition pair: (o_cond) ");
      print_gimple_stmt (dump_file, last_stmt (outer_cond_bb), 0, TDF_NONE);
      fprintf (dump_file, " (i_cond) ");
      print_gimple_stmt (dump_file, last_stmt (inner_cond_bb), 0, TDF_NONE);
    }

  basic_block then_bb1
      = make_two_separate_calls (outer_cond_bb, inner_cond_bb, then_bb);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (then_bb1)
	fprintf (dump_file,
		 "Successfully transformed: bb<%d> is a copy of bb<%d> \n",
		 then_bb1->index, then_bb->index);
      else
	fprintf (dump_file, "No transformation: bb<%d> cannot be duplicated \n",
		 then_bb->index);
    }
}

//-------------------------------------------------------------------------
// Main logic
//-------------------------------------------------------------------------
/* If cond_bb suits if-then-else pattern and got single pred, execute func
 * over it and its then, else basic blocks.  */
template <typename F>
static void
process_bb (basic_block cond_bb, F func)
{
  basic_block then_bb = NULL, else_bb = NULL;

  if (!recognize_if_then_else (cond_bb, &then_bb, &else_bb))
    return;

  func (cond_bb, then_bb, else_bb);
}

/* For each block, if it has condition, execute function over it.  We walk
 * the blocks in order that guarantees that a block with a single predecessor
 * is processed after the predecessor.  */
template <typename F>
static void
execute_function_over_conditional_bbs (F func)
{
  basic_block *bbs = single_pred_before_succ_order ();
  for (int i = n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS - 1; i >= 0; i--)
    {
      gimple *stmt = last_stmt (bbs[i]);

      if (stmt && gimple_code (stmt) == GIMPLE_COND)
	{
	  process_bb (bbs[i], func);
	}
    }
  update_ssa (TODO_update_ssa);
  free (bbs);
}

static void
process_if_split_cfun ()
{
  /* First pass.  Split complex conditions, so process_condition_pair_bb ()
   * will be able to recognize more necessary patterns.  */
  execute_function_over_conditional_bbs (process_complex_cond);

  /* Second pass.  Search each basic block for condition pair we may be
   * able to optimize.  */
  execute_function_over_conditional_bbs (
      [] (basic_block cond_bb, basic_block then_bb, basic_block else_bb)
      {
	if (!single_pred_p (cond_bb))
	  return;
	process_cond_pair (single_pred (cond_bb), cond_bb, then_bb);
      });
}

namespace
{

const pass_data pass_data_if_split = {
  GIMPLE_PASS,	    /* type.  */
  "if-split",	    /* name.  */
  OPTGROUP_NONE,    /* optinfo_flags.  */
  TV_TREE_IF_SPLIT, /* tv_id.  */
  0,		    /* properties_required.  */
  0,		    /* properties_provided.  */
  0,		    /* properties_destroyed.  */
  0,		    /* todo_flags_start.  */
  0		    /* todo_flags_finish.  */
};

class pass_if_split : public gimple_opt_pass
{
public:
  pass_if_split (gcc::context *ctxt)
      : gimple_opt_pass (pass_data_if_split, ctxt)
  {
  }

  /* opt_pass methods: */
  virtual bool
  gate (function *)
  {
    /* Don't bother doing anything if the program has errors.  */
    return (optimize >= 3 && flag_if_split && !seen_error ());
  }

  virtual unsigned int execute (function *);

}; // class pass_if_split

unsigned int
pass_if_split::execute (function *fun)
{
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  initialize_original_copy_tables ();

  process_if_split_cfun ();

  checking_verify_ssa (true, true);
  checking_verify_flow_info ();
  checking_verify_loop_structure ();
  checking_verify_dominators (CDI_DOMINATORS);
  checking_verify_dominators (CDI_POST_DOMINATORS);

  free_original_copy_tables ();
  free_dominance_info (CDI_POST_DOMINATORS);
  free_dominance_info (CDI_DOMINATORS);

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_if_split (gcc::context *ctxt)
{
  return new pass_if_split (ctxt);
}
