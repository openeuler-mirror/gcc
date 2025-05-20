/* IPA optimization to transform global calloced array to be
   specific function local.
   Copyright (C) 2021-2022 Free Software Foundation, Inc.

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
#include "gimple-iterator.h"
#include "ssa.h"
#include "tree-pass.h"
#include "tree-cfg.h"
#include "gimplify.h"
#include "gimple-pretty-print.h"
#include "tree-into-ssa.h"
#include "ipa-utils.h"
#include "fold-const.h"
#include "tree-dfa.h"
#include "cfgloop.h"

class array_localizer
{
public:
  array_localizer (varpool_node *var);
  void localize ();

private:
  bool scalar_type_p (tree type);
  bool scalar_memop_p (tree ref_val, gimple *use_stmt);
  bool stmt_dominated_by_p (enum cdi_direction dir, gimple *stmt0,
			    gimple *stmt1);
  gimple *find_calloc_stmt (gimple *stmt);
  gimple *find_free_stmt (tree var);
  bool check_var_store ();
  bool check_var_load ();
  bool find_call_edge ();
  void remove_referring_stmt (gimple *stmt);
  void replace_store_with_ssa (gimple *stmt, tree var_ssa);
  void replace_load_with_ssa (gimple *stmt, tree var_ssa);
  gimple *copy_call_without_location (gimple *stmt);
  void rewrite_array ();
  void insert_new_init (tree var_ssa);
  void insert_new_alloc_free (tree var_ssa);
  void rewrite_access_in_callee (tree var_ssa);
  void remove_orig_alloc_free ();

private:
  varpool_node *var = nullptr;
  tree var_type = nullptr;
  ipa_ref *alloc_ref = nullptr;
  ipa_ref *free_ref = nullptr;
  gimple *alloc_stmt = nullptr;
  gimple *free_stmt = nullptr;
  cgraph_node *caller = nullptr;
  cgraph_node *callee = nullptr;
  cgraph_edge *call_edge = nullptr;
  gimple *call_stmt = nullptr;

  bool scalar_alloc_p = false;

  auto_vec<gimple *> removed_stmts;
};

array_localizer::array_localizer (varpool_node *var)
  : var (var)
{
}

void
array_localizer::localize ()
{
  if (DECL_EXTERNAL (var->decl) || var->in_other_partition
      || !var->can_remove_if_no_refs_p ())
    return;

  var_type = TREE_TYPE (var->decl);

  /* Only care about pointer variable.  */
  if (!POINTER_TYPE_P (var_type))
    return;

  if (!check_var_store () || !check_var_load ())
    return;

  if (callee->used_from_other_partition
      || callee->cannot_return_p ()
      || callee->get_availability () != AVAIL_LOCAL
      || callee->has_aliases_p ())
    return;

  if (!find_call_edge ())
    return;

  {
    cfun_saver save (caller);
    if (!stmt_dominated_by_p (CDI_DOMINATORS, free_stmt, alloc_stmt)
	|| !stmt_dominated_by_p (CDI_POST_DOMINATORS, alloc_stmt, free_stmt)
	|| !stmt_dominated_by_p (CDI_POST_DOMINATORS, alloc_stmt, call_stmt)
	|| !stmt_dominated_by_p (CDI_DOMINATORS, free_stmt, call_stmt))
      return;
  }

  rewrite_array ();
}

bool
array_localizer::scalar_type_p (tree type)
{
  if (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type))
    return true;
  return false;
}

bool
array_localizer::scalar_memop_p (tree ref_val, gimple *use_stmt)
{
  if (gimple_has_volatile_ops (use_stmt))
    return false;

  if (!gimple_assign_load_p (use_stmt) && !gimple_store_p (use_stmt))
    return false;

  tree type = TREE_TYPE (ref_val);
  if (!POINTER_TYPE_P (type))
    return false;

  tree lhs = gimple_get_lhs (use_stmt);
  tree rhs1 = gimple_assign_rhs1 (use_stmt);
  tree memref = gimple_store_p (use_stmt) ? lhs : rhs1;

  HOST_WIDE_INT offset, size;
  bool reverse;
  memref = get_ref_base_and_extent_hwi (memref, &offset, &size, &reverse);

  if (!memref || offset || TREE_CODE (memref) != MEM_REF
      || !operand_equal_p (TREE_OPERAND (memref, 0), ref_val)
      || !integer_zerop (TREE_OPERAND (memref, 1))
      || !types_compatible_p (TREE_TYPE (lhs), TREE_TYPE (type)))
    return false;

  /* Exclude address-escape case like *var = var  */
  ssa_op_iter iter;
  tree use = nullptr;
  int use_count = 0;
  FOR_EACH_SSA_TREE_OPERAND (use, use_stmt, iter, SSA_OP_USE)
    if (operand_equal_p (use, ref_val) && use_count++)
      return false;

  return true;
}

bool
array_localizer::stmt_dominated_by_p (enum cdi_direction dir, gimple *stmt0,
				      gimple *stmt1)
{
  basic_block bb0 = gimple_bb (stmt0);
  basic_block bb1 = gimple_bb (stmt1);

  if (bb0 == bb1)
    {
      renumber_gimple_stmt_uids_in_blocks (&bb0, 1);

      if (dir == CDI_DOMINATORS)
	return stmt0->uid > stmt1->uid;
      else
	return stmt0->uid < stmt1->uid;
    }

  return dominated_by_p (dir, bb0, bb1);
}

gimple *
array_localizer::find_calloc_stmt (gimple *stmt)
{
  if (!gimple_assign_single_p (stmt))
    return nullptr;

  tree rhs = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (rhs) != SSA_NAME || !has_single_use (rhs))
    return nullptr;

  gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);
  if (!gimple_call_builtin_p (def_stmt, BUILT_IN_CALLOC))
    return nullptr;

  return def_stmt;
}

gimple *
array_localizer::find_free_stmt (tree var)
{
  use_operand_p use_p = nullptr;
  gimple *use_stmt = nullptr;
  if (TREE_CODE (var) != SSA_NAME || !single_imm_use (var, &use_p, &use_stmt))
    return nullptr;

  if (!gimple_call_builtin_p (use_stmt, BUILT_IN_FREE))
    return nullptr;

  return use_stmt;
}

bool
array_localizer::check_var_store ()
{
  ipa_ref *ref = nullptr;
  for (unsigned i = 0; var->iterate_referring (i, ref); i++)
    {
      cgraph_node *node = dyn_cast<cgraph_node *> (ref->referring);
      if (!node)
	return false;

      if (!ref->stmt || gimple_has_volatile_ops (ref->stmt))
	return false;

      /* Only allow calloc.  */
      if (ref->use == IPA_REF_STORE)
	{
	  /* Multiple alloc is not supported yet.  */
	  if (alloc_ref)
	    return false;

	  if (!gimple_store_p (ref->stmt)
	      || !operand_equal_p (var->decl, gimple_get_lhs (ref->stmt)))
	    return false;

	  alloc_stmt = find_calloc_stmt (ref->stmt);
	  if (!alloc_stmt)
	    return false;

	  tree arg0 = gimple_call_arg (alloc_stmt, 0);
	  tree arg1 = gimple_call_arg (alloc_stmt, 1);
	  if (TREE_CODE (arg0) != INTEGER_CST
	      || TREE_CODE (arg1) != INTEGER_CST)
	    return false;

	  tree elem_size = TYPE_SIZE_UNIT (TREE_TYPE (var_type));
	  if (scalar_type_p (TREE_TYPE (var_type))
	      && integer_onep (arg0)
	      && tree_int_cst_equal (arg1, elem_size))
	    scalar_alloc_p = true;

	  alloc_ref = ref;
	  caller = node;
	}
    }

  return alloc_ref != nullptr;
}

bool
array_localizer::check_var_load ()
{
  ipa_ref *ref = nullptr;
  for (unsigned i = 0; var->iterate_referring (i, ref); i++)
    {
      if (ref->use == IPA_REF_STORE)
	continue;

      if (ref->use != IPA_REF_LOAD)
	return false;

      if (!gimple_assign_load_p (ref->stmt)
	  || !operand_equal_p (var->decl, gimple_assign_rhs1 (ref->stmt)))
	return false;

      tree lhs = gimple_assign_lhs (ref->stmt);
      if (TREE_CODE (lhs) != SSA_NAME)
	return false;

      if (!free_ref)
	{
	  gimple *stmt = find_free_stmt (lhs);
	  if (stmt)
	    {
	      if (!operand_equal_p (gimple_call_arg (stmt, 0), lhs)
		  || ref->referring != caller)
		return false;

	      free_ref = ref;
	      free_stmt = stmt;
	      continue;
	    }
	}

      gimple *use_stmt = nullptr;
      imm_use_iterator iter;
      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	{
	  if (is_gimple_debug (use_stmt))
	    continue;

	  if (!is_gimple_assign (use_stmt))
	    return false;

	  if (scalar_alloc_p
	      && !scalar_memop_p (lhs, use_stmt))
	    scalar_alloc_p = false;

	  /* All other reference must be in the same callee.  */
	  cgraph_node *node = dyn_cast<cgraph_node *> (ref->referring);
	  if (!node || (callee && callee != node))
	    return false;

	  callee = node;
	}
    }

  return callee && callee != caller;
}

/* Now we only allow function that is called only once by other
   function (non-recursive call).  */

bool
array_localizer::find_call_edge ()
{
  cgraph_edge *e = callee->callers;
  if (!e || e->next_caller || e->caller != caller)
    return false;

  call_edge = e;
  call_stmt = e->call_stmt;
  return true;
}

void
array_localizer::remove_referring_stmt (gimple *stmt)
{
  gimple_stmt_iterator gsi;

  if (dump_file)
    {
      fprintf (dump_file, "Remove statement:\n");
      print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS);
      fprintf (dump_file, "\n");
    }

  gsi = gsi_for_stmt (stmt);
  unlink_stmt_vdef (stmt);
  gsi_remove (&gsi, true);
  release_defs (stmt);
}

void
array_localizer::replace_store_with_ssa (gimple *stmt, tree var_ssa)
{
  if (dump_file)
    {
      fprintf (dump_file, "Update store statement:\n");
      print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS);
    }

  create_new_def_for (var_ssa, stmt, NULL);
  update_stmt (stmt);

  if (dump_file)
    {
      fprintf (dump_file, "->\n");
      print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS);
      fprintf (dump_file, "\n");
    }
}

void
array_localizer::replace_load_with_ssa (gimple *stmt, tree var_ssa)
{
  if (dump_file)
    {
      fprintf (dump_file, "Update load statement:\n");
      print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS);
    }

  gimple_assign_set_rhs1 (stmt, var_ssa);
  update_stmt (stmt);

  if (dump_file)
    {
      fprintf (dump_file, "->\n");
      print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS);
      fprintf (dump_file, "\n");
    }
}

gimple *
array_localizer::copy_call_without_location (gimple *stmt)
{
  tree callee = unshare_expr_without_location (gimple_call_fndecl (stmt));
  auto_vec<tree> args;

  for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
    {
      tree arg = gimple_call_arg (stmt, i);
      args.safe_push (unshare_expr_without_location (arg));
    }

  return gimple_build_call_vec (callee, args);
}

void
array_localizer::rewrite_array ()
{
  if (dump_file)
    {
      fprintf (dump_file, "Localize global array: ");
      print_generic_expr (dump_file, var->decl);
      fprintf (dump_file, "\n\n");
    }

  cfun_saver save (callee);

  tree type = TREE_TYPE (scalar_alloc_p ? var_type : var->decl);
  const char *name = get_name (var->decl);
  tree var_ssa = make_temp_ssa_name (type, NULL, name ? name : "");

  if (scalar_alloc_p)
    insert_new_init (var_ssa);
  else
    insert_new_alloc_free (var_ssa);

  rewrite_access_in_callee (var_ssa);
  remove_orig_alloc_free ();

  for (auto stmt : removed_stmts)
    caller->remove_stmt_references (stmt);
}

void
array_localizer::insert_new_init (tree var_ssa)
{
  tree init_value = build_zero_cst (TREE_TYPE (var_ssa));
  gimple *init = gimple_build_assign (var_ssa, init_value);

  basic_block entry_bb = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  gimple_stmt_iterator gsi = gsi_start_nondebug_after_labels_bb (entry_bb);
  gsi_insert_before (&gsi, init, GSI_SAME_STMT);
}

void
array_localizer::insert_new_alloc_free (tree var_ssa)
{
  gimple *new_alloc = copy_call_without_location (alloc_stmt);
  gimple *new_free = copy_call_without_location (free_stmt);
  basic_block entry_bb = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  gimple_stmt_iterator gsi = gsi_start_nondebug_after_labels_bb (entry_bb);

  gimple_set_lhs (new_alloc, var_ssa);
  gsi_insert_before (&gsi, new_alloc, GSI_SAME_STMT);

  if (dump_file)
    {
      fprintf (dump_file, "Insert calloc statement:\n");
      print_gimple_stmt (dump_file, new_alloc, 0, TDF_VOPS);
      fprintf (dump_file, "\n");
    }

  bool free_used = false;
  for (auto e : EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      gimple *stmt = last_stmt (e->src);
      if (gimple_code (stmt) != GIMPLE_RETURN)
	continue;

      if (free_used)
	new_free = gimple_copy (new_free);
      else
	free_used = true;

      auto gsi = gsi_for_stmt (stmt);
      gimple_call_set_arg (new_free, 0, var_ssa);
      gsi_insert_before (&gsi, new_free, GSI_SAME_STMT);

      if (dump_file)
	{
	  fprintf (dump_file, "Insert free statement:\n");
	  print_gimple_stmt (dump_file, new_free, 0, TDF_VOPS);
	  fprintf (dump_file, "\n");
	}
    }
}

void
array_localizer::rewrite_access_in_callee (tree var_ssa)
{
  ipa_ref *ref = nullptr;
  for (unsigned i = 0; var->iterate_referring (i, ref); i++)
    {
      if (ref == alloc_ref || ref == free_ref)
	continue;

      gcc_assert (ref->referring == callee && ref->use == IPA_REF_LOAD);

      if (scalar_alloc_p)
	{
	  tree lhs = gimple_assign_lhs (ref->stmt);
	  gimple *use_stmt = nullptr;
	  imm_use_iterator iter;
	  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	    {
	      if (is_gimple_debug (use_stmt))
		remove_referring_stmt (use_stmt);
	      else if (gimple_store_p (use_stmt))
		replace_store_with_ssa (use_stmt, var_ssa);
	      else if (gimple_assign_load_p (use_stmt))
		replace_load_with_ssa (use_stmt, var_ssa);
	      else
		gcc_unreachable ();
	    }
	  remove_referring_stmt (ref->stmt);
	}
      else
	replace_load_with_ssa (ref->stmt, var_ssa);

      removed_stmts.safe_push (ref->stmt);
    }

  update_ssa (TODO_update_ssa);
}

void
array_localizer::remove_orig_alloc_free ()
{
  cfun_saver save (caller);

  /* Remove calloc() and free().  */
  remove_referring_stmt (alloc_stmt);
  remove_referring_stmt (alloc_ref->stmt);
  remove_referring_stmt (free_stmt);
  remove_referring_stmt (free_ref->stmt);
  removed_stmts.safe_push (alloc_ref->stmt);
  removed_stmts.safe_push (free_ref->stmt);

  update_ssa (TODO_update_ssa);
}

/* Execute the driver for IPA variable localization.  */

static unsigned int
ipa_localize_array (void)
{
  cgraph_node *node = nullptr;
  FOR_EACH_FUNCTION (node)
    {
      if (!node->real_symbol_p () || !node->definition
	  || !node->has_gimple_body_p () || node->inlined_to)
	continue;
      node->get_body ();
    }

  varpool_node *var = nullptr;
  FOR_EACH_VARIABLE (var)
    array_localizer (var).localize ();

  return 0;
}

namespace {

const pass_data pass_data_ipa_localize_array =
{
  SIMPLE_IPA_PASS, /* type */
  "localize-array",  /* name */
  OPTGROUP_NONE,   /* optinfo_flags */
  TV_IPA_LOCALIZE_ARRAY, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_localize_array : public simple_ipa_opt_pass
{
public:
  pass_ipa_localize_array (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_localize_array, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return optimize >= 3 && flag_ipa_localize_array;
    }

  virtual unsigned int execute (function *) { return ipa_localize_array (); }

}; // class pass_ipa_localize_array

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_localize_array (gcc::context *ctxt)
{
  return new pass_ipa_localize_array (ctxt);
}
