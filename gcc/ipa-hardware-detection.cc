/* Hardware Detection.
   Copyright (C) 2024-2024 Free Software Foundation, Inc.
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
#include "gimplify-me.h"
#include "ai4c-infer.h"

namespace {

/* Build a binary operation and gimplify it.  Emit code before GSI.
   Return the gimple_val holding the result.  */

static tree
gimplify_build2 (gimple_stmt_iterator *gsi, enum tree_code code,
		 tree type, tree a, tree b)
{
  tree ret;

  ret = fold_build2_loc (gimple_location (gsi_stmt (*gsi)), code, type, a, b);
  return force_gimple_operand_gsi (gsi, ret, true, NULL, true,
				   GSI_SAME_STMT);
}

static basic_block
create_abort_bb (basic_block last_bb)
{
  basic_block bb = create_empty_bb (last_bb);
  if (last_bb->loop_father != NULL)
    {
      add_bb_to_loop (bb, last_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  tree fn = builtin_decl_implicit (BUILT_IN_ABORT);
  gimple *g = gimple_build_call (fn, 0);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  return bb;
}

static basic_block
create_part_bb (basic_block last_bb, tree part_base)
{
  basic_block bb = create_empty_bb (last_bb);
  if (last_bb->loop_father != NULL)
    {
      add_bb_to_loop (bb, last_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, gimple_build_nop (), GSI_NEW_STMT);
  /* This number is used to efficiently identify the supported part range.  */
  tree part_cond = gimplify_build2 (
		     &gsi, PLUS_EXPR, unsigned_type_node, part_base,
		     build_int_cst (unsigned_type_node, 4294963967));
  gcond *cond = gimple_build_cond (LE_EXPR, part_cond,
				   build_int_cst (unsigned_type_node, 128),
				   NULL_TREE, NULL_TREE);
  gimple_set_location (cond, input_location);
  gsi_insert_before (&gsi, cond, GSI_SAME_STMT);
  gsi_remove (&gsi, true);
  return bb;
}

static void
create_detection_bb ()
{
  edge old_e = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  basic_block ret_bb = old_e->dest;

  basic_block detection_bb = create_empty_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->loop_father != NULL)
    {
      add_bb_to_loop (detection_bb, ENTRY_BLOCK_PTR_FOR_FN (cfun)->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }
  tree cpuid_decl = build_decl (input_location, VAR_DECL,
				get_identifier ("cpuid"), unsigned_type_node);
  add_local_decl (cfun, cpuid_decl);

  gimple_stmt_iterator gsi = gsi_last_bb (detection_bb);
  vec<tree, va_gc> *outputs = NULL;
  tree purpose = build_string (strlen ("=r"), "=r");
  tree output = build_tree_list (
		  build_tree_list (NULL_TREE, purpose), cpuid_decl);
  vec_safe_push (outputs, output);
  gasm *asm_stmt = gimple_build_asm_vec (
		     "mrs %0, MIDR_EL1", NULL, outputs, NULL, NULL);
  gsi_insert_after (&gsi, asm_stmt, GSI_NEW_STMT);
  gsi_insert_after (&gsi, gimple_build_nop (), GSI_NEW_STMT);

  tree implementer = gimplify_build2 (
		       &gsi, RSHIFT_EXPR, unsigned_type_node, cpuid_decl,
		       build_int_cst (unsigned_type_node, 24));
  tree part_base = gimplify_build2 (
		     &gsi, RSHIFT_EXPR, unsigned_type_node, cpuid_decl,
		     build_int_cst (unsigned_type_node, 4));
  tree part = gimplify_build2 (
		&gsi, BIT_AND_EXPR, unsigned_type_node, part_base,
		build_int_cst (unsigned_type_node, 4095));
  gcond *implementer_cond = gimple_build_cond (
			      EQ_EXPR, implementer,
			      build_int_cst (unsigned_type_node, 72),
			      NULL_TREE, NULL_TREE);
  gimple_set_location (implementer_cond, input_location);
  gsi_insert_before (&gsi, implementer_cond, GSI_SAME_STMT);
  gsi_remove (&gsi, true);

  basic_block part_bb = create_part_bb (detection_bb, part);
  basic_block abort_bb = create_abort_bb (part_bb);

  remove_edge_raw (old_e);
  make_single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun),
			 detection_bb, EDGE_FALLTHRU);
  edge etrue = make_edge (detection_bb, part_bb, EDGE_TRUE_VALUE);
  etrue->probability = profile_probability::likely ();
  edge efalse = make_edge (detection_bb, abort_bb, EDGE_FALSE_VALUE);
  efalse->probability = profile_probability::unlikely ();
  edge part_true = make_edge (part_bb, ret_bb, EDGE_TRUE_VALUE);
  part_true->probability = profile_probability::likely ();
  edge part_false = make_edge (part_bb, abort_bb, EDGE_FALSE_VALUE);
  part_false->probability = profile_probability::unlikely ();
  make_single_succ_edge (abort_bb, ret_bb, EDGE_FALLTHRU);
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, part_bb, detection_bb);
      set_immediate_dominator (CDI_DOMINATORS, ret_bb, detection_bb);
      set_immediate_dominator (CDI_DOMINATORS, abort_bb, detection_bb);
    }
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
  const char *ai_guided = getenv ("AI_GUIDED");
  return (ai_guided || (ai_infer_level
	  && optimize_maximum > 0
	  /* Only enable in lto or whole_program.  */
	  && (in_lto_p || flag_whole_program)));
}

unsigned int
pass_ipa_hardware_detection::execute (function *)
{
  unsigned int ret = 0;
  cgraph_node *cnode;
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
	      calculate_dominance_info (CDI_DOMINATORS);

	      create_detection_bb ();

	      cgraph_edge::rebuild_edges ();
	      free_dominance_info (CDI_DOMINATORS);
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
