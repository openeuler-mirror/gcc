/* replace the std::find with sve.
   Copyright (C) 2005-2022 Free Software Foundation, Inc.

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
#include "tree.h"
#include "function.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-pass.h"
#include "context.h"
#include "target.h"
#include "toplev.h"
#include "cfghooks.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "gimple-ssa.h"
#include "gimple-pretty-print.h"
#if defined (__aarch64__) && !defined (CROSS_DIRECTORY_STRUCTURE)
#include "config/aarch64/aarch64.h"
#endif

namespace {

#define TRACE_FUNCTION(fun)\
  if (dump_file)\
  {\
    fprintf (dump_file, "\nprocess function: \n");\
    dump_function_to_file (fun, dump_file, TDF_NONE);\
    fprintf (dump_file, "\n");\
  }

#define TRACE_STMT(stmt)\
  if (dump_file)\
  {\
    fprintf (dump_file, "\nprocess stmt: \n");\
    print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);\
    fprintf (dump_file, "\n");\
  }

#define TRACE_REPLACE_STMT(stmt)\
  if (dump_file)\
  {\
    fprintf (dump_file, "\nprocess replace stmt: \n");\
    print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);\
    fprintf (dump_file, "\n");\
  }

#define TRACE_ARG3_TYPE(type)\
  if (dump_file)\
  {\
    fprintf (dump_file, "\nprocess arg3 type: \n");\
    dump_node (type, TDF_NONE, dump_file);\
    fprintf (dump_file, "\n");\
  }

const pass_data pass_data_find_with_sve = {
  GIMPLE_PASS, /* type.  */
  "find_with_sve", /* name.  */
  OPTGROUP_NONE, /* optinfo_flags.  */
  TV_NONE, /* tv_id.  */
  0, /* properties_required.  */
  0, /* properties_provided.  */
  0, /* properties_destroyed.  */
  0, /* todo_flags_start.  */
  TODO_cleanup_cfg | TODO_update_ssa | TODO_update_address_taken
  | TODO_rebuild_cgraph_edges, /* todo_flags_finish.  */
};

class pass_find_with_sve : public gimple_opt_pass {
public:
  pass_find_with_sve (gcc::context *ctx) :
    gimple_opt_pass (pass_data_find_with_sve, ctx)
  {}

  virtual bool gate (function *fun) override
  {
#if defined (__aarch64__) && !defined (CROSS_DIRECTORY_STRUCTURE)
    if (!flag_find_with_sve)
      return false;

    if (!TARGET_SVE)
      return false;

    if (!targetm.vector_mode_supported_p (V2DImode))
      return false;

    return true;
#else
    return false;
#endif
  }

virtual unsigned int execute (function *fun) override
{
#if defined (__aarch64__) && !defined (CROSS_DIRECTORY_STRUCTURE)
  TRACE_FUNCTION (fun->decl);
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
  {
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
      !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (std_find_check (stmt))
	replace_std_find (gsi);
    }
  }
#endif
  return 0;
}

private:
#if defined (__aarch64__) && !defined (CROSS_DIRECTORY_STRUCTURE)
  uint8_t bit_width;
  const char *null_name = "";

  bool std_find_check (gimple *stmt)
  {
    if (!is_gimple_call (stmt))
      return false;

    tree fndecl = gimple_call_fndecl (stmt);
    if (fndecl == nullptr || DECL_NAME (fndecl) == nullptr)
      return false;

    const char *fn_name = IDENTIFIER_POINTER (DECL_NAME (fndecl));
    if (fn_name == nullptr || strcmp (fn_name, "find") != 0)
      return false;

    if (DECL_CONTEXT (fndecl) == nullptr
      || TREE_CODE (DECL_CONTEXT (fndecl)) != NAMESPACE_DECL)
      return false;

    const char *namespace_name
      = IDENTIFIER_POINTER (DECL_NAME (DECL_CONTEXT (fndecl)));
    if (namespace_name == nullptr || strcmp (namespace_name, "std") != 0)
      return false;

    /* Exclude the scenarios : xxx::std::find.  */
    if (DECL_CONTEXT (DECL_CONTEXT (fndecl))
	&& TREE_CODE (DECL_CONTEXT (DECL_CONTEXT (fndecl)))
	== NAMESPACE_DECL)
      return false;

    if (gimple_call_num_args (stmt) != 3)
      return false;

    tree arg1 = DECL_ARGUMENTS (fndecl);
    tree arg2 = TREE_CHAIN (arg1);
    tree arg3 = TREE_CHAIN (arg2);

    tree arg3_type = TREE_TYPE (arg3);
    if (TREE_CODE (arg3_type) != REFERENCE_TYPE)
      return false;

    tree main_type = TREE_TYPE (arg3_type);
    TRACE_ARG3_TYPE (main_type);
    if (TREE_CODE (main_type) == INTEGER_TYPE)
    {
      if (TYPE_PRECISION (main_type) != 64)
	return false;

      const char *type_name = get_type_name_arg (main_type);
      if ((strcmp (type_name, "long unsigned int") != 0)
	&& (strcmp (type_name, "long int") != 0))
	return false;

      this->bit_width = 64;
    } else if (TREE_CODE (main_type) == POINTER_TYPE)
      this->bit_width = 64;
    else
      return false;

    tree arg1_type = TREE_TYPE (arg1);
    if (TREE_CODE (arg1_type) == POINTER_TYPE)
      return true;
    else if (TREE_CODE (arg1_type) == RECORD_TYPE)
    {
      const char *type_name = get_type_name_arg (arg1_type);
      if (strcmp (type_name, "__normal_iterator") == 0)
	return true;
    }

    return false;
  }

  const char *get_type_name_arg (tree main_type)
  {
    enum tree_code code = TREE_CODE (main_type);
    enum tree_code_class tclass = TREE_CODE_CLASS (code);

    if (tclass == tcc_type)
    {
      if (TYPE_NAME (main_type))
      {
	if (TREE_CODE (TYPE_NAME (main_type)) == IDENTIFIER_NODE)
	{
	  const char *type_name = IDENTIFIER_POINTER (
	    TYPE_NAME (main_type));
	  if (type_name)
	    return type_name;
	}
	else if (TREE_CODE (TYPE_NAME (main_type)) == TYPE_DECL
	    && DECL_NAME (TYPE_NAME (main_type)))
	{
	  const char *type_name = IDENTIFIER_POINTER (
	    DECL_NAME (TYPE_NAME (main_type)));
	  if (type_name)
	    return type_name;
	}
      }
    }

    return null_name;
  }

  void replace_std_find (gimple_stmt_iterator gsi)
  {
    switch (this->bit_width)
    {
      case 64:
	replace_std_find_u64 (gsi);
	break;
      case 32:
      case 16:
      case 8:
      default:;
    }
  }

  void replace_std_find_u64 (gimple_stmt_iterator gsi)
  {
    gimple *stmt = gsi_stmt (gsi);
    tree old_fndecl = gimple_call_fndecl (stmt);
    TRACE_STMT (stmt);

    // arguments list process:
    auto_vec<tree, 4> args;
    for (unsigned i = 0; i < gimple_call_num_args (stmt); ++i)
      args.safe_push (gimple_call_arg (stmt, i));
    tree new_arg = build_int_cst (unsigned_char_type_node,
      sve_expand_std_find_threshold);
    args.safe_push (new_arg);

    // functon declare process:
    tree old_type = TREE_TYPE (old_fndecl);
    tree ret_type = TREE_TYPE (old_type);
    tree arg_types = NULL_TREE;
    for (tree t = TYPE_ARG_TYPES (old_type); t; t = TREE_CHAIN (t))
      arg_types = tree_cons (NULL_TREE, TREE_VALUE (t), arg_types);
    arg_types = tree_cons (NULL_TREE, unsigned_char_type_node, arg_types);
    arg_types = nreverse (arg_types);
    tree new_fndecl_type = build_function_type (ret_type, arg_types);
    tree new_fndecl = build_fn_decl ("__sve_optimized_find_u64",
      new_fndecl_type);
    TREE_PUBLIC (new_fndecl) = 1;
    DECL_EXTERNAL (new_fndecl) = 1;

    // call function process:
    gcall *new_call = gimple_build_call_vec (new_fndecl, args);
    if (gimple_has_lhs (stmt))
      gimple_call_set_lhs (new_call, gimple_call_lhs (stmt));
    gsi_replace (&gsi, new_call, true);
    update_stmt (gsi_stmt (gsi));
    TRACE_REPLACE_STMT (gsi_stmt (gsi));
  }
#endif
};
}  // namespace

gimple_opt_pass *make_pass_find_with_sve (gcc::context *ctx)
{
  return new pass_find_with_sve (ctx);
}
