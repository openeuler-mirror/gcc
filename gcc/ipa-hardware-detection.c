/* Hardware Detection.
   Copyright (C) 2022-2022 Free Software Foundation, Inc.
This file is part of GCC.
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "ssa.h"
#include "tree-into-ssa.h"
#include "cfganal.h"
#include "cfgloop.h"
#include "gimple-pretty-print.h"
#include "tree-cfg.h"
#include "cgraph.h"
#include "print-tree.h"
#include "cfghooks.h"
#include "gimple-fold.h"
#include "basic-block.h"

namespace {

/* Get the target function.  */
bool 
target_func_p (tree fn_decl, const char* target)
{
  const char *fn_name = IDENTIFIER_POINTER (DECL_NAME (fn_decl));
  return (fn_name && sizeof (fn_name) == sizeof (target)  
	  && strncmp (fn_name, target, sizeof (target) - 1) == 0);
}


const pass_data pass_data_ipa_hardware_detection =
{
  SIMPLE_IPA_PASS,
  "hardware_detection",
  OPTGROUP_NONE,
  TV_IPA_HARDWARE_DETECTION,
  (PROP_cfg | PROP_ssa),
  0,
  0,
  0,
  (TODO_update_ssa | TODO_verify_all)
};

class pass_ipa_hardware_detection : public simple_ipa_opt_pass
{
public:
  pass_ipa_hardware_detection (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_hardware_detection, ctxt)
  {}

  virtual bool gate (function *);
  virtual unsigned int execute (function *);
}; // class pass_ipa_hardware_detection

bool
pass_ipa_hardware_detection::gate (function *)
{
  const char *ai_infer_level = getenv ("AI_INFER_LEVEL");
  const char *ai_lto_option = getenv ("AI_LTO_OPTION");
  return ((ai_lto_option || (ai_infer_level && optimize_machine > 0)) && (in_lto_p || flag_whole_program));
}

unsigned int
pass_ipa_hardware_detection::execute (function *)
{
  unsigned int ret = 0;
  cgraph_node *cnode;
  gcall* call_stmt = NULL;
  tree fntype_void_void = build_function_type_array (void_type_node, 0, NULL);
  tree fndecl_decl = build_fn_decl ("get_ai_info", fntype_void_void);

  DECL_EXTERNAL (fndecl_decl) = 1;
  TREE_PUBLIC (fndecl_decl) = 1;
  DECL_CONTEXT (fndecl_decl) = NULL;
  struct cgraph_node *node = cgraph_node::create (fndecl_decl);

  FOR_EACH_FUNCTION (cnode)
    {
      const char *func_name = IDENTIFIER_POINTER (DECL_NAME (cnode->decl));
      if (target_func_p (cnode->decl, "get_ai_info"))
        {
          call_stmt = gimple_build_call (cnode->decl, 0);
          break;
        }
    }

  FOR_EACH_FUNCTION (cnode)
    {
      if (!cnode->real_symbol_p ())
	{
	  continue;
	}
      if (cnode->definition)
	{
	  if (!cnode->has_gimple_body_p () || cnode->inlined_to)
	      continue;

	  cnode->get_body ();
	  function *fn = DECL_STRUCT_FUNCTION (cnode->decl);
	  if (!fn)
	      continue;

	  if (DECL_NAME (cnode->decl)
      	      && MAIN_NAME_P (DECL_NAME (cnode->decl)))
	    {
	      push_cfun (fn);
	      basic_block first_block = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
	      gimple_stmt_iterator gsi = gsi_start_bb (first_block);
	      if (call_stmt)
		gsi_insert_before (&gsi, call_stmt, GSI_NEW_STMT); 
	      pop_cfun ();
	    }
	}
    }
  return ret;
}
} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_hardware_detection (gcc::context *ctxt)
{
  return new pass_ipa_hardware_detection (ctxt);
}
