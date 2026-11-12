/* Copyright (C) 2019-2022 Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "tree-cfg.h"
#include "tree-pass.h"
#include "tm_p.h"
#include "basic-block.h"
#include "bitmap.h"
#include "function.h"
#include "cfg.h"
#include "cgraph.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "gimple-ssa.h"
#include "ipa-utils.h"

class alignment_propagator
{
public:
  alignment_propagator (cgraph_node *node);

  void execute ();

private:
  void propagate_params_alignment ();
  void transform ();
  size_t get_param_alignment (unsigned param_index);
  size_t get_var_alignment (tree var);
  bool check_assign (gimple *stmt, auto_vec<tree> &worklist,
		     size_t &alignment);
  bool check_param (tree t, auto_vec<tree> &worklist, size_t &alignment);
  int get_param_index_from_ssa (tree var);
  size_t get_arg_alignment (cgraph_node *caller, tree arg);
  size_t new_alignment (size_t orig, size_t new_value);
  bool pow2_or_zerop (size_t value);
  size_t abs_value (tree t);
  bool candidate_stmt_p (gimple *stmt);

private:
  cgraph_node *node = nullptr;
  hash_map<tree, size_t> alignment_map;
};

alignment_propagator::alignment_propagator (cgraph_node *node)
  : node (node)
{
}

void
alignment_propagator::execute ()
{
  if (dump_file)
    {
      fprintf (dump_file, "Start to rewrite function: %s\n",
	       node->dump_asm_name());
      dump_function_to_file (node->decl, dump_file, dump_flags);
    }

  cfun_saver save (node);

  propagate_params_alignment ();

  /* If no alignment is propagated, there is no need to continue cause
     the rest cases are covered by constant propagation.  */
  if (!alignment_map.is_empty ())
    transform ();
}

void
alignment_propagator::propagate_params_alignment ()
{
  unsigned i = 0;
  tree param = DECL_ARGUMENTS (node->decl);
  while (param)
    {
      size_t alignment = get_param_alignment (i);
      if (alignment)
	alignment_map.put (param, alignment);

      param = DECL_CHAIN (param);
      i++;
    }
}

void
alignment_propagator::transform ()
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (auto gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (!candidate_stmt_p (stmt))
	    continue;

	  if (dump_file)
	    {
	      fprintf (dump_file, "Rewrite stmt:\n  ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);
	    }

	  tree lhs = gimple_assign_lhs (stmt);
	  tree new_rhs = build_int_cst (TREE_TYPE (lhs), 0);
	  gimple_assign_set_rhs_from_tree (&gsi, new_rhs);
	  update_stmt (stmt);

	  if (dump_file)
	    {
	      fprintf (dump_file, "To:\n  ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);
	      fprintf (dump_file, "\n");
	    }
	}
    }
}

size_t
alignment_propagator::get_param_alignment (unsigned param_index)
{
  size_t alignment = 0;
  for (auto e = node->callers; e; e = e->next_caller)
    {
      if (e->caller == node)
	continue;

      if (gimple_call_num_args (e->call_stmt) <= param_index)
	return 0;

      tree arg = gimple_call_arg (e->call_stmt, param_index);
      size_t arg_alignment = get_arg_alignment (e->caller, arg);
      if (!arg_alignment)
	return 0;

      if (!alignment || arg_alignment < alignment)
	alignment = arg_alignment;
    }

  return alignment;
}

size_t
alignment_propagator::get_var_alignment (tree var)
{
  size_t alignment = 0;

  auto_bitmap visited;
  auto_vec<tree> worklist;
  worklist.safe_push (var);

  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();
      if (TREE_CODE (t) == INTEGER_CST)
	{
	  size_t value = abs_value (t);
	  if (!pow2_or_zerop (value))
	    return 0;

	  alignment = new_alignment (alignment, value);
	  continue;
	}

      if (TREE_CODE (t) != SSA_NAME)
	return 0;

      if (!bitmap_set_bit (visited, SSA_NAME_VERSION (t)))
	continue;

      gimple *stmt = SSA_NAME_DEF_STMT (t);
      switch (gimple_code (stmt))
	{
	  case GIMPLE_PHI:
	    for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
	      worklist.safe_push (gimple_phi_arg_def (stmt, i));
	    break;
	  case GIMPLE_ASSIGN:
	    if (!check_assign (stmt, worklist, alignment))
	      return 0;
	    break;
	  case GIMPLE_NOP:
	    /* If we reach a default def, try to get the argument's alignment
	       from caller node.  */
	    if (!check_param (t, worklist, alignment))
	      return 0;
	    break;
	  default:
	    return 0;
	}
    }

  return alignment;
}

bool
alignment_propagator::check_assign (gimple *stmt, auto_vec<tree> &worklist,
				    size_t &alignment)
{
  if (gimple_assign_single_p (stmt) || gimple_assign_cast_p (stmt))
    {
      worklist.safe_push (gimple_assign_rhs1 (stmt));
      return true;
    }

  switch (gimple_assign_rhs_code (stmt))
    {
      case NEGATE_EXPR:
	worklist.safe_push (gimple_assign_rhs1 (stmt));
	return true;
      case MAX_EXPR:
	[[fallthrough]];
      case MIN_EXPR:
	[[fallthrough]];
      case POINTER_PLUS_EXPR:
	[[fallthrough]];
      case POINTER_DIFF_EXPR:
	[[fallthrough]];
      case PLUS_EXPR:
	[[fallthrough]];
      case MINUS_EXPR:
	worklist.safe_push (gimple_assign_rhs1 (stmt));
	worklist.safe_push (gimple_assign_rhs2 (stmt));
	return true;
      case MULT_EXPR:
	break;
      default:
	return false;
    }

  /* For mult_expr, rhs2 must be an integer constant, so we can simply take
     this constant as alignment.  Otherwise, return false.  */
  tree rhs2 = gimple_assign_rhs2 (stmt);
  if (TREE_CODE (rhs2) != INTEGER_CST)
    return false;

  alignment = new_alignment (alignment, abs_value (rhs2));
  return true;
}

bool
alignment_propagator::check_param (tree t, auto_vec<tree> &worklist,
				   size_t &alignment)
{
  int index = get_param_index_from_ssa (t);
  if (index == -1)
    return false;

  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    {
      if (gimple_call_num_args (e->call_stmt) <= index)
	return false;

      tree arg = gimple_call_arg (e->call_stmt, index);
      if (e->caller == node)
	worklist.safe_push (arg);
      else
	{
	  auto *align = alignment_map.get (SSA_NAME_VAR (t));
	  if (!align)
	    return false;

	  alignment = new_alignment (alignment, *align);
	}
    }

  return true;
}

/* Find param from VAR and return its index.  Return -1 if fail.  */

int
alignment_propagator::get_param_index_from_ssa (tree var)
{
  if (!SSA_NAME_IS_DEFAULT_DEF (var) || !SSA_NAME_VAR (var))
    return -1;

  tree param = DECL_ARGUMENTS (cfun->decl);
  int index = 0;
  while (param && param != SSA_NAME_VAR (var))
    {
      param = DECL_CHAIN (param);
      index++;
    }

  return index;
}

/* Get alignment of an argument if it is calculated from the address of a
   local variable.  */

size_t
alignment_propagator::get_arg_alignment (cgraph_node *caller, tree arg)
{
  if (!caller || !arg)
    return 0;

  cfun_saver save (caller);

  tree base = nullptr;
  tree offset = nullptr;

  /* Extract base and offset.  */
  if (TREE_CODE (arg) == ADDR_EXPR)
    {
      base = arg;
      tree op0 = TREE_OPERAND (base, 0);
      if (TREE_CODE (op0) == MEM_REF)
	{
	  base = TREE_OPERAND (op0, 0);
	  offset = TREE_OPERAND (op0, 1);
	}
    }
  else
    {
      if (TREE_CODE (arg) != SSA_NAME)
	return 0;

      gimple *stmt = SSA_NAME_DEF_STMT (arg);
      if (!is_gimple_assign (stmt))
	return 0;

      if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
	offset = gimple_assign_rhs2 (stmt);
      else if (!gimple_assign_single_p (stmt))
	return 0;

      base = gimple_assign_rhs1 (stmt);
    }

  /* Check if ARG uses the address of a local variable.  */
  if (TREE_CODE (base) != ADDR_EXPR)
    return 0;

  tree decl = TREE_OPERAND (base, 0);
  if (!decl || !VAR_P (decl)
      || decl_function_context (decl) != current_function_decl)
    return 0;

  size_t alignment = LOCAL_DECL_ALIGNMENT (decl) / 8;

  /* Update alignment if there is an offset.  */
  if (offset)
    {
      if (TREE_CODE (offset) != INTEGER_CST)
	return 0;

      auto value = abs_value (offset);
      if (!pow2_or_zerop (value))
	return 0;

      alignment = new_alignment (alignment, value);
    }

  return alignment;
}

size_t
alignment_propagator::new_alignment (size_t orig_value, size_t new_value)
{
  if (!new_value)
    return orig_value;

  if (!orig_value || new_value < orig_value)
    return new_value;

  return orig_value;
}

bool
alignment_propagator::pow2_or_zerop (size_t value)
{
  return !(value & (value - 1));
}

size_t
alignment_propagator::abs_value (tree t)
{
  gcc_assert (TREE_CODE (t) == INTEGER_CST);
  auto value = TREE_INT_CST_LOW (t);

  return std::abs (static_cast <HOST_WIDE_INT> (value));
}

bool
alignment_propagator::candidate_stmt_p (gimple *stmt)
{
  if (!is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != BIT_AND_EXPR)
    return false;

  tree var = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);

  return rhs2 && TREE_CODE (rhs2) == INTEGER_CST
	 && TREE_INT_CST_LOW (rhs2) < get_var_alignment (var);
}


static unsigned
ipa_propagate_alignment (void)
{
  auto_vec<cgraph_node *> candidate_nodes;
  cgraph_node *cnode = NULL;
  FOR_EACH_FUNCTION (cnode)
    {
      if (!cnode->real_symbol_p () || !cnode->definition
	  || !cnode->has_gimple_body_p () || cnode->inlined_to)
	continue;

      cnode->get_body ();
      candidate_nodes.safe_push (cnode);
    }

  for (auto *node : candidate_nodes)
    alignment_propagator (node).execute ();

  return 0;
}

namespace {
const pass_data pass_data_ipa_alignment_propagation = {
  SIMPLE_IPA_PASS,
  "alignment-propagation",
  OPTGROUP_NONE,
  TV_IPA_ALIGNMENT_PROPAGATION,
  (PROP_cfg | PROP_ssa),
  0,
  0,
  (TODO_update_ssa),
  (TODO_verify_all),
};

class pass_ipa_alignment_propagation
  : public simple_ipa_opt_pass
{
public:
  pass_ipa_alignment_propagation (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_alignment_propagation, ctxt)
  {}

  virtual bool gate (function *)
  {
    return optimize >= 3 && flag_ipa_alignment_propagation;
  }

  virtual unsigned execute (function *)
  {
    return ipa_propagate_alignment ();
  }
};
} /* namespace.  */

simple_ipa_opt_pass *
make_pass_ipa_alignment_propagation (gcc::context *ctxt)
{
  return new pass_ipa_alignment_propagation (ctxt);
}
