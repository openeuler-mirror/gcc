/* Array dead store elimination
   Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

#include "ipa-array-dse.h"

#include "basic-block.h"
#include "bitmap.h"
#include "cgraph.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "cfg.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-builder.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "gimple-ssa.h"
#include "gimple-walk.h"
#include "gimplify-me.h"
#include "ipa-utils.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "tree-pretty-print.h"
#include "tree-ssanames.h"
#include "tree-vrp.h"
#include "tree.h"

namespace array_dse {

#define RANGE_TYPE long_long_integer_type_node
#define RANGE_INF LONG_LONG_MAX
#define RANGE_NINF LONG_LONG_MIN

static inline bool
integer_cst_p (tree t)
{
  return TREE_CODE (t) == INTEGER_CST && !TREE_OVERFLOW (t);
}

static tree
strip_base (tree addr)
{
  tree base = get_base_address (addr);
  return TREE_CODE (base) == MEM_REF ? TREE_OPERAND (base, 0) : nullptr;
}

static tree
strip_ssa_copy (tree var)
{
  if (!var || TREE_CODE (var) != SSA_NAME)
    return var;

  while (true)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (var);
      if (!gimple_assign_single_p (stmt) && !gimple_assign_cast_p (stmt))
	break;

      tree rhs = gimple_assign_rhs1 (stmt);
      if (!rhs || TREE_CODE (rhs) != SSA_NAME)
	break;

      var = rhs;
    }

  return var;
}

static inline unsigned
greatest_common_divisor (unsigned a, unsigned b)
{
  return b == 0 ? a : greatest_common_divisor (b, a % b);
}

static compare_result
opposite_compare_result (compare_result result)
{
  switch (result)
    {
      case COMPARE_ERROR: return COMPARE_ERROR;
      case LT: return GT;
      case EQ: return NE;
      case GT: return LT;
      case LE: return GE;
      case GE: return LE;
      case NE: return EQ;
    }
}

static tree_code
opposite_cond_code (tree_code code)
{
  switch (code)
    {
      case LT_EXPR: return GE_EXPR;
      case LE_EXPR: return GT_EXPR;
      case GT_EXPR: return LE_EXPR;
      case GE_EXPR: return LT_EXPR;
      case EQ_EXPR: return NE_EXPR;
      case NE_EXPR: return EQ_EXPR;
      default:
	return ERROR_MARK;
    }
}

/* Calculate step of a loop variable, record all stmts that plus step.  */

static int
calc_loop_var_step (tree loop_var, tree iterate_var,
		    hash_set<gimple *> *iterate_stmts = nullptr)
{
  int step = 0;
  auto_bitmap visited;
  auto_vec<tree> worklist;
  worklist.safe_push (iterate_var);

  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();
      if (TREE_CODE (t) != SSA_NAME)
	return 0;

      if (t == loop_var || !bitmap_set_bit (visited, SSA_NAME_VERSION (t)))
	continue;

      gimple *stmt = SSA_NAME_DEF_STMT (t);
      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
	    worklist.safe_push (gimple_phi_arg_def (stmt, i));
	  continue;
	}

      /* Check iterate stmts' pattern: _2 = _1 + step.  */
      if (!is_gimple_assign (stmt)
	  || (gimple_assign_rhs_code (stmt) != POINTER_PLUS_EXPR
	      && gimple_assign_rhs_code (stmt) != PLUS_EXPR))
	return 0;

      tree ptr = gimple_assign_rhs1 (stmt);
      tree offset = gimple_assign_rhs2 (stmt);
      if (TREE_CODE (offset) != INTEGER_CST)
	return 0;

      worklist.safe_push (ptr);
      HOST_WIDE_INT offset_val = TREE_INT_CST_LOW (offset);
      if (step && offset_val != step)
	return 0;
      step = offset_val;

      if (iterate_stmts)
	iterate_stmts->add (stmt);
    }

  return step;
}

/* VAR is a loop var when:
     1. VAR is defined by a phi in LOOP's header.
     2. The defination phi should have two args, one comes from preheader
	and the other comes from latch.  */

static bool
loop_var_p (loop_p loop, tree var)
{
  if (TREE_CODE (var) != SSA_NAME)
    return false;

  gimple *stmt = SSA_NAME_DEF_STMT (var);
  if (gimple_code (stmt) != GIMPLE_PHI || gimple_bb (stmt) != loop->header)
    return false;

  edge preheader_edge = loop_preheader_edge (loop);
  edge latch_edge = loop_latch_edge (loop);

  return preheader_edge && latch_edge
	 && PHI_ARG_DEF_FROM_EDGE (stmt, preheader_edge)
	 && PHI_ARG_DEF_FROM_EDGE (stmt, latch_edge);
}

static inline tree
build_value (HOST_WIDE_INT value)
{
  return build_int_cst (RANGE_TYPE, value);
}

static inline value_range
make_range (HOST_WIDE_INT value)
{
  tree v = build_value (value);
  return value_range{v, v};
}

static inline value_range
make_range (HOST_WIDE_INT min, HOST_WIDE_INT max)
{
  return value_range{build_value (min), build_value (max)};
}

static infinite_kind
infinite_p (tree value)
{
  tree type = TREE_TYPE (value);
  if (TREE_CODE (value) != INTEGER_CST || TYPE_PRECISION (type) == 1)
    return infinite_kind::NON_INF;

  wide_int type_min = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
  wide_int type_max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));

  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
      && wi::to_wide (value) == type_min)
    return infinite_kind::NINF;

  if (wi::to_wide (value) == type_max)
    return infinite_kind::INF;

  return infinite_kind::NON_INF;
}

static inline HOST_WIDE_INT
get_multiplier (tree t)
{
  if (TREE_CODE (t) != MULT_EXPR
      || TREE_CODE (TREE_OPERAND (t, 1)) != INTEGER_CST)
    return 0;

  return TREE_INT_CST_LOW (TREE_OPERAND (t, 1));
}

static tree negate_tree (tree t);
static tree minus_tree (tree t1, tree t2);

/* Convert negative multiplier to positive.  */

static void
handle_negate_multiplier (tree t)
{
  HOST_WIDE_INT multiplier = get_multiplier (t);
  if (multiplier >= 0)
    return;

  tree lhs = TREE_OPERAND (t, 0);
  if (TREE_CODE (lhs) == PLUS_EXPR)
    {
      TREE_OPERAND (t, 0) = minus_tree (negate_tree (TREE_OPERAND (lhs, 0)),
					TREE_OPERAND (lhs, 1));
      TREE_OPERAND (t, 1) = build_int_cst (RANGE_TYPE, -multiplier);
    }
  else if (TREE_CODE (lhs) == MINUS_EXPR)
    {
      TREE_OPERAND (t, 0) = negate_tree (lhs);
      TREE_OPERAND (t, 1) = build_int_cst (RANGE_TYPE, -multiplier);
    }
}

static tree
negate_tree (tree t)
{
  if (!t)
    return nullptr;

  if (infinite_p (t) == infinite_kind::INF)
    return build_value (RANGE_NINF);
  else if (infinite_p (t) == infinite_kind::NINF)
    return build_value (RANGE_INF);
  else
    return fold_build1 (NEGATE_EXPR, RANGE_TYPE, t);
}

static tree
plus_tree (tree t1, tree t2)
{
  if (!t1 || !t2)
    return nullptr;

  infinite_kind inf1 = infinite_p (t1);
  infinite_kind inf2 = infinite_p (t2);

  if ((inf1 == infinite_kind::INF && inf2 == infinite_kind::NINF)
      || (inf1 == infinite_kind::NINF && inf2 == infinite_kind::INF))
    return nullptr;

  if (inf1 == infinite_kind::NINF || inf2 == infinite_kind::NINF)
    return build_value (RANGE_NINF);

  if (inf1 == infinite_kind::INF || inf2 == infinite_kind::INF)
    return build_value (RANGE_INF);

  tree ret = fold_build2 (PLUS_EXPR, RANGE_TYPE, t1, t2);
  handle_negate_multiplier (ret);

  return ret;
}

static tree
minus_tree (tree t1, tree t2)
{
  if (!t1 || !t2)
    return nullptr;

  infinite_kind inf1 = infinite_p (t1);
  infinite_kind inf2 = infinite_p (t2);

  if ((inf1 == infinite_kind::INF && inf2 == infinite_kind::INF)
      || (inf1 == infinite_kind::NINF && inf2 == infinite_kind::NINF))
    return nullptr;

  if (inf1 == infinite_kind::NINF || inf2 == infinite_kind::INF)
    return build_value (RANGE_NINF);

  if (inf1 == infinite_kind::INF || inf2 == infinite_kind::NINF)
    return build_value (RANGE_INF);

  tree ret = fold_build2 (MINUS_EXPR, RANGE_TYPE, t1, t2);
  handle_negate_multiplier (ret);

  return ret;
}

/* Callback for walk_tree, usage:
     walk_tree (&A, sub_expr_p, B, nullptr)

   Check if B is sub expr of A.
 */

static tree
sub_expr_p (tree *opnd_ptr, int *walk_subtrees ATTRIBUTE_UNUSED, void *data)
{
  tree opnd = *opnd_ptr;
  tree var = static_cast<tree> (data);

  if (opnd == var)
    return var;

  return NULL_TREE;
}

static unsigned
get_ptr_layers (tree expr)
{
  unsigned layers = 0;
  while (POINTER_TYPE_P (expr) || TREE_CODE (expr) == ARRAY_TYPE)
    {
      layers++;
      expr = TREE_TYPE (expr);
    }

  return layers;
}

static bool
find_base (gimple *stmt ATTRIBUTE_UNUSED, tree base,
	   tree var ATTRIBUTE_UNUSED, void *data)
{
  return (TREE_CODE (base) == MEM_REF
	  && TREE_OPERAND (base, 0) == static_cast<tree> (data));
}

static bool
gimple_phi_arg_p (gimple *stmt, tree var)
{
  for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
    if (gimple_phi_arg_def (stmt, i) == var)
      return true;

  return false;
}

/* Returns the number of FIELD_DECLs in TYPE.  */

static unsigned
fields_length (const_tree type)
{
  tree t = TYPE_FIELDS (type);
  return list_length (t);
}

/* Get unique base address of VAR.  */

tree
addr_analyzer::get_address (tree var)
{
  if (tree *it = address_map.get (var))
    return *it;

  tree addr = analyze_address (var);
  address_map.put (var, addr);

  return addr;
}

/* Try to find the unique base address to which VAR is accessing in
   current function.  */

tree
addr_analyzer::analyze_address (tree var)
{
  tree addr = nullptr;
  auto_bitmap visited;
  auto_vec<tree> worklist;
  worklist.safe_push (var);

  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();
      if (TREE_CODE (t) != SSA_NAME || !POINTER_TYPE_P (TREE_TYPE (t)))
	return nullptr;

      if (!bitmap_set_bit (visited, SSA_NAME_VERSION (t)))
	continue;

      if (SSA_NAME_IS_DEFAULT_DEF (t))
	{
	  tree new_addr = SSA_NAME_VAR (t);
	  if (!new_addr || (addr && addr != new_addr))
	    return nullptr;

	  addr = new_addr;
	  continue;
	}

      gimple *def_stmt = SSA_NAME_DEF_STMT (t);
      if (is_gimple_assign (def_stmt))
	{
	  if (!gimple_assign_single_p (def_stmt)
	      && !gimple_assign_cast_p (def_stmt)
	      && gimple_assign_rhs_code (def_stmt) != POINTER_PLUS_EXPR)
	    return nullptr;

	  worklist.safe_push (gimple_assign_rhs1 (def_stmt));
	}
      else if (gimple_code (def_stmt) == GIMPLE_PHI)
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (def_stmt); i++)
	    worklist.safe_push (gimple_phi_arg_def (def_stmt, i));
	}
      else
	return nullptr;
    }

  return addr;
}

array_dse_callee::array_dse_callee (cgraph_node *node)
  : node (node)
{
}

/* Check if the node could be a candidate callee for array dse.  */

bool
array_dse_callee::analyze ()
{
  cfun_saver save (node, LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);

  return filter_function () && find_candidate_array ()
	 && find_length_param () && check_array_usage ();
}

unsigned HOST_WIDE_INT
array_dse_callee::get_len_param_max () const
{
  return len_param_max;
}

tree
array_dse_callee::mult_tree (basic_block bb, tree t1, tree t2)
{
  if (!bb || !t1 || !t2 || !integer_cst_p (t2)
      || infinite_p (t2) != infinite_kind::NON_INF)
    return nullptr;

  if (integer_zerop (t1) || integer_zerop (t2))
    return integer_zero_node;

  auto range1 = calc_tree_range (bb, t1);
  if (tree_to_shwi (range1.min ()) < 0)
    return nullptr;

  HOST_WIDE_INT multiplier = tree_to_shwi (t2);
  if (infinite_p (t1) != infinite_kind::NON_INF)
    return build_value (multiplier > 0 ? RANGE_INF : RANGE_NINF);

  return fold_build2 (MULT_EXPR, RANGE_TYPE, t1, t2);
}

tree
array_dse_callee::div_tree (basic_block bb, tree t1, tree t2)
{
  if (!bb || !t1 || !t2 || !integer_cst_p (t2)
      || infinite_p (t2) != infinite_kind::NON_INF)
    return nullptr;

  if (integer_zerop (t2))
    return nullptr;

  if (integer_zerop (t1))
    return integer_zero_node;

  auto range1 = calc_tree_range (bb, t1);
  if (tree_to_shwi (range1.min ()) < 0)
    return nullptr;

  HOST_WIDE_INT divisor = tree_to_shwi (t2);

  if (infinite_p (t1) != infinite_kind::NON_INF)
    return build_value (divisor > 0 ? RANGE_INF : RANGE_NINF);

  return fold_build2 (TRUNC_DIV_EXPR, RANGE_TYPE, t1, t2);
}

tree
array_dse_callee::lshift_tree (tree t1, tree t2)
{
  if (!t1 || !t2 || !integer_cst_p (t2))
    return nullptr;

  if (infinite_p (t1) != infinite_kind::NON_INF)
    return t1;

  return fold_build2 (LSHIFT_EXPR, RANGE_TYPE, t1, t2);
}

tree
array_dse_callee::rshift_tree (tree t1, tree t2)
{
  if (!t1 || !t2 || !integer_cst_p (t2))
    return nullptr;

  if (infinite_p (t1) != infinite_kind::NON_INF)
    return t1;

  return fold_build2 (RSHIFT_EXPR, RANGE_TYPE, t1, t2);
}

tree
array_dse_callee::max_tree (basic_block bb, tree t1, tree t2)
{
  if (!bb || !t1 || !t2)
    return nullptr;

  switch (compare_tree (bb, t1, t2))
    {
      case EQ: return t1;
      case GT: return t1;
      case GE: return t1;
      case LT: return t2;
      case LE: return t2;
      default: return nullptr;
    }
}

tree
array_dse_callee::min_tree (basic_block bb, tree t1, tree t2)
{
  if (!bb || !t1 || !t2)
    return nullptr;

  switch (compare_tree (bb, t1, t2))
    {
      case EQ: return t2;
      case GT: return t2;
      case GE: return t2;
      case LT: return t1;
      case LE: return t1;
      default: return nullptr;
    }
}

/* Calculate the value of T, where T is an expression with len_main_var and
   N_VALUE is len_main_var's value.  */

HOST_WIDE_INT
array_dse_callee::calc_tree_value (tree t, HOST_WIDE_INT n_value)
{
  if (TREE_CODE (t) == INTEGER_CST)
    return tree_to_shwi (t);

  if (t == len_main_var || t == signed_len_var)
    return n_value;

  HOST_WIDE_INT op_value[2];
  for (int i = 0; i < std::min (2, tree_operand_length (t)); i++)
    op_value[i] = calc_tree_value (TREE_OPERAND (t, i), n_value);

  switch (TREE_CODE (t))
    {
      case NEGATE_EXPR:
	return -op_value[0];
      case PLUS_EXPR:
	return op_value[0] + op_value[1];
      case MINUS_EXPR:
	return op_value[0] - op_value[1];
      case MULT_EXPR:
	return op_value[0] * op_value[1];
      case TRUNC_DIV_EXPR:
	return op_value[0] / op_value[1];
      case LSHIFT_EXPR:
	return op_value[0] * (1 << op_value[1]);
      case RSHIFT_EXPR:
	return op_value[0] / (1 << op_value[1]);
      default:
	return 0;
    }
}

/* Calculate expression T's range.  */

value_range
array_dse_callee::calc_tree_range (basic_block bb, tree t)
{
  if (!t)
    return value_range{RANGE_TYPE};

  if (TREE_CODE (t) == INTEGER_CST)
    return make_range (tree_to_shwi (t));

  if (t == len_main_var || t == signed_len_var)
    return len_range_map.get_or_insert (bb);

  int len = tree_operand_length (t);
  gcc_assert (len > 0);
  value_range range1 = calc_tree_range (bb, TREE_OPERAND (t, 0));
  value_range range2;
  if (len == 2)
    range2 = calc_tree_range (bb, TREE_OPERAND (t, 1));

  switch (TREE_CODE (t))
    {
      /* Since the variable in both expressions is len_main_var and both
	 expressions are monotonically increasing, we can just substitute
	 the maximum and minimum values of len_main_var to calculate the
	 expression T's range.  */
      case PLUS_EXPR:
      case MINUS_EXPR:
	{
	  tree op[2] = {TREE_OPERAND (t, 0), TREE_OPERAND (t, 1)};
	  if (integer_cst_p (op[0]) || integer_cst_p (op[1]))
	    break;

	  auto len_range = len_range_map.get_or_insert (bb);
	  auto len_min = tree_to_shwi (len_range.min ());
	  auto len_max = tree_to_shwi (len_range.max ());
	  auto min1 = calc_tree_value (op[0], len_min);
	  auto max1 = calc_tree_value (op[0], len_max);
	  auto min2 = calc_tree_value (op[1], len_min);
	  auto max2 = calc_tree_value (op[1], len_max);

	  auto min = TREE_CODE (t) == PLUS_EXPR ? min1 + min2 : min1 - min2;
	  auto max = TREE_CODE (t) == PLUS_EXPR ? max1 + max2 : max1 - max2;

	  if (min > max)
	    std::swap (min, max);

	  return make_range (min, max);
	}
      default:
	break;
    }

  return build_range (bb, TREE_CODE (t), range1, range2);
}

value_range
array_dse_callee::build_range (basic_block bb, tree_code op,
			       const value_range &r1, const value_range &r2)
{
  tree min = nullptr;
  tree max = nullptr;
  switch (op)
    {
      case NEGATE_EXPR:
	min = negate_tree (r1.max ());
	max = negate_tree (r1.min ());
	break;
      case PLUS_EXPR:
	[[fallthrough]];
      case POINTER_PLUS_EXPR:
	min = plus_tree (r1.min (), r2.min ());
	max = plus_tree (r1.max (), r2.max ());
	break;
      case MINUS_EXPR:
	[[fallthrough]];
      case POINTER_DIFF_EXPR:
	min = minus_tree (r1.min(), r2.max ());
	max = minus_tree (r1.max(), r2.min ());
	break;
      case MULT_EXPR:
	min = mult_tree (bb, r1.min (), r2.min ());
	max = mult_tree (bb, r1.max (), r2.max ());
	break;
      case TRUNC_DIV_EXPR:
	min = div_tree (bb, r1.min (), r2.max ());
	max = div_tree (bb, r1.max (), r2.min ());
	break;
      case LSHIFT_EXPR:
	min = lshift_tree (r1.min (), r2.min ());
	max = lshift_tree (r1.max (), r2.max ());
	break;
      case RSHIFT_EXPR:
	min = rshift_tree (r1.min (), r2.max ());
	max = rshift_tree (r1.max (), r2.min ());
	break;
      case MAX_EXPR:
	min = max_tree (bb, r1.min (), r2.min ());
	max = max_tree (bb, r1.max (), r2.max ());
	break;
      case MIN_EXPR:
	min = min_tree (bb, r1.min (), r2.min ());
	max = min_tree (bb, r1.max (), r2.max ());
	break;
      default:
	break;
    }

  return min && max ? value_range{min, max} : value_range{RANGE_TYPE};
}

/* Compare two pointer range value in BB.  */

compare_result
array_dse_callee::compare_tree (basic_block bb, tree t1, tree t2)
{
  if (!bb || !t1 || !t2)
    return COMPARE_ERROR;

  if (operand_equal_p (t1, t2))
    return EQ;

  auto ret = compare_tree_by_minus (bb, t1, t2);
  if (!ret)
    ret = opposite_compare_result (compare_tree_by_minus (bb, t2, t1));

  return ret;
}

compare_result
array_dse_callee::compare_tree_by_minus (basic_block bb, tree t1, tree t2)
{
  tree expr = minus_tree (t1, t2);
  auto range = calc_tree_range (bb, expr);
  HOST_WIDE_INT min = tree_to_shwi (range.min ());
  HOST_WIDE_INT max = tree_to_shwi (range.max ());
  if (min == 0)
    return GE;
  if (min > 0)
    return GT;
  if (max == 0)
    return LE;
  if (max < 0)
    return LT;

  return COMPARE_ERROR;
}

bool
array_dse_callee::filter_function () const
{
  return leaf_recursive_node_p (node) && no_return_p ()
	 /* There must be two params: array and length.  */
	 && list_length (DECL_ARGUMENTS (node->decl)) == PARAM_NUM;
}

/* Candidate callee must return no value.  Each return block can't have any
   stmt except a return stmt.  */

bool
array_dse_callee::no_return_p () const
{
  tree return_type = TREE_TYPE (TREE_TYPE (cfun->decl));
  if (TREE_CODE (return_type) != VOID_TYPE)
    return false;

  for (auto return_edge : EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      basic_block return_bb = return_edge->src;
      if (!single_succ_p (return_bb))
	return false;

      gimple *stmt = first_stmt (return_bb);
      if (gimple_code (stmt) != GIMPLE_RETURN
	  || gimple_return_retval (as_a<greturn *> (stmt)))
	return false;
    }

  return true;
}

bool
array_dse_callee::find_main_vars ()
{
  auto_bitmap visited;
  tree default_def[PARAM_NUM] = {nullptr, nullptr};

  /* Collect all params' default def.  */
  unsigned i;
  tree name;
  FOR_EACH_SSA_NAME (i, name, cfun)
    {
      if (!SSA_NAME_IS_DEFAULT_DEF (name)
	  || SSA_NAME_IS_VIRTUAL_OPERAND (name))
	continue;

      gimple *stmt = SSA_NAME_DEF_STMT (name);
      if (gimple_code (stmt) != GIMPLE_NOP)
	return false;

      /* Each param should have an unique default ssa def.  */
      int index = find_param_index (SSA_NAME_VAR (name));
      if (index == -1 || !bitmap_set_bit (visited, index))
	return false;

      default_def[index] = name;
    }

  if (bitmap_count_bits (visited) != PARAM_NUM)
    return false;

  array_main_var = default_def[array_param_index];
  len_main_var = default_def[len_param_index];

  find_tail_recursive_loop (default_def);

  signed_len_var = fold_convert (RANGE_TYPE, len_main_var);

  return true;
}

/* Try to find a tail recursive loop.  */

void
array_dse_callee::find_tail_recursive_loop (tree *default_def)
{
  tree main_loop_var[PARAM_NUM] = {nullptr, nullptr};
  loop_p unique_loop = nullptr;

  for (unsigned i = 0; i < PARAM_NUM; i++)
    {
      tree name = default_def[i];

      use_operand_p use_p;
      gimple *stmt = nullptr;
      if (!single_imm_use (name, &use_p, &stmt)
	  || gimple_code (stmt) != GIMPLE_PHI)
	return;

      main_loop_var[i] = gimple_phi_result (stmt);

      /* Check if all main vars are defined in the same loop header.  */
      basic_block bb = gimple_bb (stmt);
      loop_p loop = bb->loop_father;
      if (!loop || loop->num == 0 || !bb_loop_header_p (bb)
	  || (unique_loop && unique_loop != loop))
	return;

      unique_loop = loop;
    }

  /* Multiple latch is not allow.  */
  if (!unique_loop || !loop_latch_edge (unique_loop))
    return;

  /* The loop header must be the "first" block.  There shouldn't be any
     stmt before entering main loop.  */
  basic_block entry_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  basic_block preheader = loop_preheader_edge (unique_loop)->src;
  if (single_succ_p (preheader) && single_pred_p (preheader)
      && single_pred (preheader) == entry_bb && single_succ_p (entry_bb)
      && empty_block_p (preheader))
    {
      main_loop = unique_loop;
      array_main_var = main_loop_var[array_param_index];
      len_main_var = main_loop_var[len_param_index];
    }
}

/* Check if the function only store to the array passed by its param.  */

bool
array_dse_callee::find_candidate_array ()
{
  tree unique_array = nullptr;
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (auto gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (gimple_clobber_p (stmt))
	    continue;

	  /* There are 3 kind of stmts may have store ops: GIMPLE_ASSIGN,
	     GIMPLE_CALL and GIMPLE_ASM.  */
	  if (gimple_has_volatile_ops (stmt)
	      || gimple_code (stmt) == GIMPLE_ASM)
	    return false;

	  /* We have check that current function only has recursive call,
	     and it doesn't return a value, so we can skip call stmt.  */
	  if (gimple_code (stmt) != GIMPLE_ASSIGN)
	    continue;

	  tree lhs = gimple_assign_lhs (stmt);
	  if (TREE_CODE (lhs) == SSA_NAME)
	    continue;

	  tree base = strip_base (lhs);
	  if (!base || TREE_CODE (base) != SSA_NAME)
	    return false;

	  tree array = analyzer.get_address (base);
	  if (!array || (unique_array && unique_array != array))
	    return false;

	  unique_array = array;
	}
    }

  if (!unique_array)
    return false;

  int index = find_param_index (unique_array);
  if (index < 0)
    return false;

  array_param = unique_array;
  array_param_index = index;

  if (dump_file)
    {
      fprintf (dump_file, "Found unique stored array: ");
      print_generic_expr (dump_file, unique_array);
      fprintf (dump_file, "\n");
    }

  return true;
}

/* Check if the function has length param.  */

bool
array_dse_callee::find_length_param ()
{
  collect_read_write_ptrs ();

  tree len = nullptr;
  unsigned size = 0;
  for (auto ptr : all_ptrs)
    if (!check_pointer (ptr, len, size))
      return false;

  if (!len || TREE_CODE (len) != SSA_NAME || !SSA_NAME_VAR (len))
    return false;

  int index = find_param_index (SSA_NAME_VAR (len));
  if (index < 0)
    return false;

  len_param = SSA_NAME_VAR (len);
  len_param_index = index;
  elem_size = build_int_cst (RANGE_TYPE, size);
  elem_size_cst = size;
  calc_length_param_max ();

  if (len && dump_file)
    {
      fprintf (dump_file, "Found unique array length: ");
      print_generic_expr (dump_file, len_param);
      fprintf (dump_file, "\n");
    }

  return true;
}

void
array_dse_callee::collect_read_write_ptrs ()
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (auto gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  for (unsigned i = 0; i < gimple_num_ops (stmt); i++)
	    {
	      tree op = gimple_op (stmt, i);
	      if (!op)
		continue;

	      tree base = strip_base (op);
	      if (!base || TREE_CODE (base) != SSA_NAME)
		continue;

	      tree array = analyzer.get_address (base);
	      if (array != array_param)
		continue;

	      all_ptrs.add (base);
	    }
	}
    }
}

/* We heuristically set upper bound of length param to the max value with
   half bits of its data type.

   TODO: Overflows may still occur, we need a better implement.
 */

void
array_dse_callee::calc_length_param_max ()
{
  unsigned bits = TYPE_PRECISION (TREE_TYPE (len_param));
  len_param_max = 1L << (bits / 2);
}

/* Check pointer pattern: ptr = ptr1 + offset1
				 |
			    ptr2 + offset2
			      |
			     ...
			      |
			ARRAY + offset3

   All ptrs we visited must be calculated by adding offset to array_param.
   All offset must be an expression with the only variable len_param.
   LEN will be set to the unique variable we founded.
   SIZE will be set to the minimum offset unit, which will be treated as the
   array element size.
 */

bool
array_dse_callee::check_pointer (tree ptr, tree &len, unsigned &size)
{
  visited_offset.empty ();
  auto_bitmap visited;
  auto_vec<tree> worklist;
  worklist.safe_push (ptr);

  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();
      if (!POINTER_TYPE_P (TREE_TYPE (t)) || TREE_CODE (t) != SSA_NAME)
	return false;

      if (!bitmap_set_bit (visited, SSA_NAME_VERSION (t)))
	continue;

      if (SSA_NAME_IS_DEFAULT_DEF (t))
	{
	  tree var = SSA_NAME_VAR (t);
	  if (!var || var != array_param)
	    return false;

	  continue;
	}

      gimple *stmt = SSA_NAME_DEF_STMT (t);
      if (is_gimple_assign (stmt))
	{
	  worklist.safe_push (gimple_assign_rhs1 (stmt));
	  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
	      && !check_offset (gimple_assign_rhs2 (stmt), len, size,
				worklist))
	    return false;
	}
      else if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
	    worklist.safe_push (gimple_phi_arg_def (stmt, i));
	}
      else
	return false;
    }

  return true;
}

/* Check offset part.  */

bool
array_dse_callee::check_offset (tree var, tree &len, unsigned &size,
				auto_vec<tree> &worklist)
{
  if (visited_offset.contains (var))
    return true;
  visited_offset.add (var);

  if (TREE_CODE (TREE_TYPE (var)) != INTEGER_TYPE)
    return false;

  tree offset = strip_ssa_copy (var);
  if (TREE_CODE (offset) == INTEGER_CST)
    {
      HOST_WIDE_INT value = TREE_INT_CST_LOW (offset);
      value = std::abs (value);
      size = size ? greatest_common_divisor (size, value) : value;
      return true;
    }

  if (TREE_CODE (offset) != SSA_NAME)
    return false;

  gimple *stmt = SSA_NAME_DEF_STMT (offset);
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
	if (!check_offset (gimple_phi_arg_def (stmt, i), len, size, worklist))
	  return false;
    }
  else if (!is_gimple_assign (stmt))
    return false;

  switch (gimple_assign_rhs_code (stmt))
    {
      case MAX_EXPR:
	[[fallthrough]];
      case MIN_EXPR:
	[[fallthrough]];
      case PLUS_EXPR:
	[[fallthrough]];
      case MINUS_EXPR:
	return check_offset (gimple_assign_rhs1 (stmt), len, size, worklist)
	       && check_offset (gimple_assign_rhs2 (stmt), len, size,
				worklist);
      case POINTER_DIFF_EXPR:
	worklist.safe_push(gimple_assign_rhs1 (stmt));
	worklist.safe_push(gimple_assign_rhs2 (stmt));
	return true;
      case NEGATE_EXPR:
	return check_offset (gimple_assign_rhs1 (stmt), len, size, worklist);
      case MULT_EXPR:
	return check_mult_expr (stmt, len, size);
      default:
	return false;
    }
}

/* Handle MULT_EXPR.  */

bool
array_dse_callee::check_mult_expr (gimple *stmt, tree &len, unsigned &size)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);

  /* Handle size.  */
  if (TREE_CODE (rhs2) != INTEGER_CST)
    return false;

  HOST_WIDE_INT value = TREE_INT_CST_LOW (rhs2);
  size = greatest_common_divisor (size, std::abs (value));

  /* Handle index.  */
  rhs1 = strip_ssa_copy (rhs1);
  if (TREE_CODE (rhs1) != SSA_NAME)
    return false;

  gimple *index_stmt = SSA_NAME_DEF_STMT (rhs1);
  if (is_gimple_assign (index_stmt) && gimple_num_ops (index_stmt) > 2)
    {
      if (TREE_CODE (gimple_assign_rhs2 (index_stmt)) != INTEGER_CST)
	return false;
      rhs1 = gimple_assign_rhs1 (index_stmt);
    }

  if (len && len != rhs1)
    return false;
  len = rhs1;

  return true;
}

/* Find the param index of VAR in current function.
   Return -1 if not found.  */

int
array_dse_callee::find_param_index (tree var)
{
  if (TREE_CODE (var) != PARM_DECL)
    return -1;

  tree param = DECL_ARGUMENTS (node->decl);
  int index = 0;
  while (param)
    {
      if (param == var)
	return index;

      param = DECL_CHAIN (param);
      index++;
    }

  return -1;
}

bool
array_dse_callee::check_array_usage ()
{
  find_main_vars ();

  return calc_ptr_range () && check_ptr_range ()
	 && check_recursive_call_arg ();
}

/* Calculate len_param's value range in each block.
   We assume its initial range is [1, len_param_max], we will validate this
   range at each call to this callee.  */

void
array_dse_callee::calc_len_range ()
{
  /* Init all blocks' len_range.  */
  auto full_len_range = make_range (len_param_min, len_param_max);
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    len_range_map.put (bb, full_len_range);

  /* Calculate new range according to condition.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple *stmt = gsi_stmt (gsi_last_bb (bb));
      auto cond_range = get_range_from_cond (stmt);
      if (cond_range.undefined_p ())
	continue;

      edge true_edge = nullptr;
      edge false_edge = nullptr;
      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);
      update_len_range (true_edge->dest, cond_range);
      update_len_range (false_edge->dest, invert_range (cond_range));
    }
}

/* Update len_param's range in all block dominated by START.  */

void
array_dse_callee::update_len_range (basic_block start,
				    const value_range &new_range)
{
  for (auto bb : get_all_dominated_blocks (CDI_DOMINATORS, start))
    (*len_range_map.get (bb)).intersect (new_range);
}

value_range
array_dse_callee::invert_range (const value_range &range) const
{
  auto new_range = range;
  new_range.invert ();

  return new_range;
}

/* Get range of len_param from a condition.  */

value_range
array_dse_callee::get_range_from_cond (gimple *stmt)
{
  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return value_range{};

  gcond *cond = as_a<gcond *> (stmt);
  tree_code code = gimple_cond_code (cond);
  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);

  if (lhs != len_main_var || TREE_CODE (rhs) != INTEGER_CST)
    return value_range{};

  HOST_WIDE_INT value = TREE_INT_CST_LOW (rhs);

  switch (code)
    {
      case LT_EXPR:
	return make_range (RANGE_NINF, value - 1);
      case LE_EXPR:
	return make_range (RANGE_NINF, value);
      case GT_EXPR:
	return make_range (value + 1, RANGE_INF);
      case GE_EXPR:
	return make_range (value, RANGE_INF);
      case EQ_EXPR:
	return make_range (value);
      case NE_EXPR:
	return invert_range (make_range (value));
      default:
	return value_range{};
    }
}

/* Get range of a variable, represented by len_param. If variable is a
   pointer, return the range of its offset from array_param.  */

value_range
array_dse_callee::get_var_range (basic_block bb, tree var)
{
  if (var == array_main_var)
    return make_range (0, 0);

  if (var == len_main_var)
    return value_range{signed_len_var, signed_len_var};

  if (find_var_range (var, bb))
    return var_range[var][bb];

  /* If we can't calculate its range, keep it varying.  */
  auto &range = var_range[var][bb];
  range.set_varying (RANGE_TYPE);

  if (TREE_CODE (var) == INTEGER_CST)
    {
      HOST_WIDE_INT value = TREE_INT_CST_LOW (var);
      range = make_range (value);
      return range;
    }

  if (TREE_CODE (var) != SSA_NAME)
    return range;

  /* Build range expression recursively.  */
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      range = get_var_range (bb, gimple_phi_arg_def (stmt, 0));
      for (unsigned i = 1; i < gimple_phi_num_args (stmt); i++)
	{
	  tree arg = gimple_phi_arg_def (stmt, i);
	  auto arg_range = get_var_range (bb, arg);
	  tree min = min_tree (bb, range.min (), arg_range.min ());
	  tree max = max_tree (bb, range.max (), arg_range.max ());
	  if (!min || !max)
	    {
	      range.set_varying (RANGE_TYPE);
	      break;
	    }

	  range = value_range{min, max};
	}
      return range;
    }

  if (!is_gimple_assign (stmt))
    return range;

  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_num_ops (stmt) > 2 ? gimple_assign_rhs2 (stmt) : nullptr;
  value_range range1 = get_var_range (bb, rhs1);
  value_range range2 = value_range{RANGE_TYPE};
  if (rhs2)
    range2 = get_var_range (bb, rhs2);

  if (gimple_assign_single_p (stmt) || gimple_assign_cast_p (stmt))
    range = range1;
  else
    range = build_range (bb, gimple_assign_rhs_code (stmt), range1, range2);

  return range;
}

/* Calculate pointer's offset range by checking loop condition.  */

bool
array_dse_callee::calc_ptr_range ()
{
  calc_len_range ();

  auto_bitmap visited;
  auto_vec<basic_block> worklist;
  loop_p l = main_loop ? main_loop : current_loops->tree_root;
  worklist.safe_push (l->header);

  while (!worklist.is_empty ())
    {
      basic_block bb = worklist.pop ();
      if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
	  || (main_loop && bb == main_loop->latch)
	  || unreachable_blocks.contains (bb))
	continue;

      if (bb->flags & BB_IRREDUCIBLE_LOOP)
	return false;

      if (!bitmap_set_bit (visited, bb->index))
	continue;

      if (loop_header_p (bb) && !calc_loop_var_range (bb->loop_father))
	  return false;

      for (auto succ : bb->succs)
	worklist.safe_push (succ->dest);
    }

  return true;
}

/* Check if offset range of all pointers calculated by array_param are
   within [0, (len_param -1) * elem_size].  */

bool
array_dse_callee::check_ptr_range ()
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (unreachable_blocks.contains (bb))
	continue;

      for (auto gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  for (unsigned i = 0; i < gimple_num_ops (stmt); i++)
	    {
	      tree op = gimple_op (stmt, i);
	      if (!op)
		continue;

	      tree base = strip_base (op);
	      if (!base || TREE_CODE (base) != SSA_NAME
		  || !all_ptrs.contains (base))
		continue;

	      auto range = get_var_range (bb, base);
	      /* offset >= 0.  */
	      auto ret = compare_tree (bb, range.min (), integer_zero_node);
	      if (!ret || ret & LT)
		return false;

	      /* offset <= (n - 1) * elem_size.  */
	      tree tmp = minus_tree (signed_len_var, integer_one_node);
	      tmp = mult_tree (bb, tmp, elem_size);
	      ret = compare_tree (bb, range.max (), tmp);
	      if (!ret || ret & GT)
		return false;
	    }
	}
    }

  return true;
}

/* Check range of recursive call arguments:
     void func(a, n) {
       ...
       func(a1, n1);
     }

     1. (1) a1 >= a (tail recursive call)
	(2) a1 == a (normal recursive call)
     2. n1 >= 1
     3. a1 + n1 * elem_size <= a + n * elem_size
 */

bool
array_dse_callee::check_recursive_call_arg ()
{
  auto_vec<tree> array_args;
  auto_vec<tree> len_args;
  auto_vec<basic_block> blocks;
  auto_vec<bool> is_tail_recursive_call;

  collect_recursive_call_args (array_args, len_args, blocks,
			       is_tail_recursive_call);

  for (unsigned i = 0; i < array_args.length (); i++)
    {
      basic_block bb = blocks[i];
      update_branch_range (bb);
      auto array_range = get_var_range (bb, array_args[i]);
      auto len_range = get_var_range (bb, len_args[i]);

      /* Check requirement 2.  */
      auto ret = compare_tree (bb, len_range.min (), integer_one_node);
      if (!ret || ret & ~GE)
	return false;

      if (is_tail_recursive_call[i])
	{
	  /* Check requirement 1.1.  */
	  ret = compare_tree (bb, array_range.min (), integer_zero_node);
	  if (!ret || ret & ~GE)
	    return false;
	}
      else
	{
	  /* Check requirement 1.2.  */
	  if (!integer_zerop (array_range.min ())
	      || !integer_zerop (array_range.max ()))
	    return false;
	}

      /* Check requirement 3.  */
      tree offset = build_recursive_offset (len_args[i]);
      if (!offset)
	return false;

      tree recursive_ptr_max
	= build_recursive_ptr_range_max (bb, array_args[i], offset);
      if (!recursive_ptr_max)
	return false;

      tree upper_bound = mult_tree (bb, signed_len_var, elem_size);
      ret = compare_tree (bb, recursive_ptr_max, upper_bound);
      if (!ret || ret & ~LE)
	return false;
    }

  return true;
}

void
array_dse_callee::collect_recursive_call_args (
  auto_vec<tree> &array_args, auto_vec<tree> &len_args,
  auto_vec<basic_block> &blocks, auto_vec<bool> &is_tail_recursive_call)
{
  for (cgraph_edge *edge = node->callees; edge; edge = edge->next_callee)
    {
      if (node != edge->callee)
	continue;

      gcall *call = edge->call_stmt;
      tree array_arg = gimple_call_arg (call, array_param_index);
      tree len_arg = gimple_call_arg (call, len_param_index);

      array_args.safe_push (array_arg);
      len_args.safe_push (len_arg);
      blocks.safe_push (gimple_bb (call));
      is_tail_recursive_call.safe_push (tail_recursive_call_p (call));
    }

  if (main_loop)
    {
      gimple *array_def_stmt = SSA_NAME_DEF_STMT (array_main_var);
      gimple *len_def_stmt = SSA_NAME_DEF_STMT (len_main_var);
      edge latch_edge = loop_latch_edge (main_loop);
      tree array_arg = PHI_ARG_DEF_FROM_EDGE (array_def_stmt, latch_edge);
      tree len_arg = PHI_ARG_DEF_FROM_EDGE (len_def_stmt, latch_edge);
      array_args.safe_push (array_arg);
      len_args.safe_push (len_arg);
      blocks.safe_push (latch_edge->src);
      is_tail_recursive_call.safe_push (true);
    }
}

/* If BB is first block after a condition jump, try to update range according
   to the condition.  */

void
array_dse_callee::update_branch_range (basic_block bb)
{
  if (!single_pred_p (bb))
    return;

  basic_block pred = single_pred (bb);
  gimple *stmt = gsi_stmt (gsi_last_bb (pred));
  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return;

  tree lhs = gimple_cond_lhs (stmt);
  tree rhs = gimple_cond_rhs (stmt);
  if (!integer_cst_p (rhs))
    return;

  tree_code code = gimple_cond_code (stmt);
  if (single_pred_edge (bb)->flags & EDGE_FALSE_VALUE)
    code = opposite_cond_code (code);

  auto range = get_var_range (bb, lhs);
  tree min = range.min ();
  tree max = range.max ();
  HOST_WIDE_INT value = TREE_INT_CST_LOW (rhs);

  switch (code)
    {
      case LT_EXPR:
	value--;
	[[fallthrough]];
      case LE_EXPR:
	max = min_tree (bb, max, build_value (value));
	break;
      case GT_EXPR:
	value++;
	[[fallthrough]];
      case GE_EXPR:
	min = max_tree (bb, min, build_value (value));
	break;
      case EQ_EXPR:
	var_range[lhs][bb] = make_range (value);
	return;
      default:
	return;
    }

  var_range[lhs][bb] = value_range{min, max};
}

/* If LEN = (ptr1 - ptr2) / elem_size,
   then recursive_offset = LEN * elem_size = (ptr1 - ptr2).

   We can do this only when ptr1 and ptr2 comes from array_param,
   so (ptr1 - ptr2) is an integer multiple of elem_size.
 */

tree
array_dse_callee::build_recursive_offset (tree len_arg)
{
  if (TREE_CODE (len_arg) != SSA_NAME)
    return nullptr;

  gimple *stmt = SSA_NAME_DEF_STMT (len_arg);
  if (!is_gimple_assign (stmt))
    return nullptr;

  /* Check pattern: (ptr1 - ptr2) / elem_size.  */
  tree_code code = gimple_assign_rhs_code (stmt);
  if (code != TRUNC_DIV_EXPR && code != RSHIFT_EXPR)
    return nullptr;

  tree rhs1 = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (rhs1) != SSA_NAME)
    return nullptr;

  gimple *def = SSA_NAME_DEF_STMT (strip_ssa_copy (rhs1));
  if (!is_gimple_assign (def)
      || gimple_assign_rhs_code (def) != POINTER_DIFF_EXPR)
    return nullptr;

  /* Check ptr1 and ptr2.  */
  tree len = nullptr;
  unsigned size = 0;
  if (!check_pointer (gimple_assign_rhs1 (def), len, size)
      || !check_pointer (gimple_assign_rhs2 (def), len, size)
      || len != len_main_var || size != elem_size_cst)
    return nullptr;

  tree rhs2 = gimple_assign_rhs2 (stmt);
  if (!integer_cst_p (rhs2))
    return nullptr;

  HOST_WIDE_INT value = TREE_INT_CST_LOW (rhs2);
  if (code == RSHIFT_EXPR)
    value = 1 << value;

  if (value != elem_size_cst)
    return nullptr;

  return rhs1;
}

/* Build expression of recursive pointer range max.  */

tree
array_dse_callee::build_recursive_ptr_range_max (basic_block bb,
						 tree array_arg,
						 tree offset)
{
  if (TREE_CODE (array_arg) != SSA_NAME)
    return nullptr;

  tree recursive_ptr_max = nullptr;
  gimple *stmt = SSA_NAME_DEF_STMT (array_arg);

  /* If ARRAY_ARG = rhs1 - offset, return rhs1's range max directly.  */
  if (is_gimple_assign (stmt)
      && gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      if (TREE_CODE (rhs2) == SSA_NAME)
	{
	  stmt = SSA_NAME_DEF_STMT (rhs2);
	  if (is_gimple_assign (stmt)
	      && gimple_assign_rhs_code (stmt) == NEGATE_EXPR
	      && gimple_assign_rhs1 (stmt) == offset)
	    recursive_ptr_max = get_var_range (bb, rhs1).max ();
	}
    }

  if (!recursive_ptr_max)
    {
      auto range1 = get_var_range (bb, array_arg);
      auto range2 = get_var_range (bb, offset);
      recursive_ptr_max = plus_tree (range1.max (), range2.max ());
    }

  return recursive_ptr_max;
}

bool
array_dse_callee::tail_recursive_call_p (gimple *stmt)
{
  if (stmt->next)
    return false;

  basic_block bb = gimple_bb (stmt);
  return single_succ_p (bb) && return_bb_p (single_succ (bb));
}

bool
array_dse_callee::return_bb_p (basic_block bb) const
{
  return bb && single_succ_p (bb) &&
	 single_succ (bb) == EXIT_BLOCK_PTR_FOR_FN (cfun);
}

/* Calculate the range of a loop variable according to initial value and
   loop exit condition.  */

bool
array_dse_callee::calc_loop_var_range (loop_p loop)
{
  if (!loops_state_satisfies_p (LOOPS_HAVE_PREHEADERS)
      && loops_state_satisfies_p (LOOPS_MAY_HAVE_MULTIPLE_LATCHES))
    return false;

  basic_block header = loop->header;
  for (auto gsi = gsi_start_phis (header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = as_a<gphi *> (gsi_stmt (gsi));
      tree result = gimple_phi_result (phi);
      if (!loop_var_p (loop, result))
	continue;

      tree iterate_var = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));
      int step = calc_loop_var_step (result, iterate_var);
      unsigned abs_step = static_cast<unsigned> (std::abs (step));
      if (!step)
	continue;

      if (POINTER_TYPE_P (TREE_TYPE (result)))
	{
	  if (abs_step != elem_size_cst)
	    return false;
	  loop_ptrs[loop].add (result);
	}
      else if (TREE_CODE (TREE_TYPE (result)) != INTEGER_TYPE
	       || abs_step != 1)
	return false;

      tree init_var = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
      auto init_range = get_var_range (header, init_var);
      tree min = step > 0 ? init_range.min () : build_value (RANGE_NINF);
      tree max = step > 0 ? build_value (RANGE_INF) : init_range.max ();
      auto new_range = value_range{min, max};
      for (auto bb : get_all_dominated_blocks (CDI_DOMINATORS, header))
	{
	  if (!find_var_range (result, bb))
	    {
	      var_range[result][bb] = new_range;
	      continue;
	    }

	  auto &range = var_range[result][bb];
	  min = max_tree (bb, min, range.min ());
	  max = min_tree (bb, max, range.max ());
	  if (!min || !max)
	    return false;
	  range = value_range {min, max};
	}
    }

  if (!check_loop_exits (loop))
    return false;

  return true;
}

bool
array_dse_callee::check_loop_exits (loop_p loop)
{
  for (auto edge : get_loop_exit_edges (loop))
    {
      gimple *stmt = gsi_stmt (gsi_last_bb (edge->src));
      if (gimple_code (stmt) != GIMPLE_COND)
	continue;

      gcond *cond = as_a<gcond *> (stmt);
      tree lhs = gimple_cond_lhs (cond);
      tree rhs = gimple_cond_rhs (cond);

      bool lhs_cand_p = loop_var_p (loop, lhs) || iterate_var_p (loop, lhs);
      bool rhs_cand_p = loop_var_p (loop, rhs) || iterate_var_p (loop, rhs);
      if (!lhs_cand_p && !rhs_cand_p)
	continue;

      tree step = nullptr;
      if (POINTER_TYPE_P (TREE_TYPE (lhs))
	  && POINTER_TYPE_P (TREE_TYPE (rhs)))
	{
	  if (TREE_CODE (lhs) != SSA_NAME || TREE_CODE (rhs) != SSA_NAME)
	    return false;
	  step = elem_size;
	}
      else if (TREE_CODE (TREE_TYPE (lhs)) == INTEGER_TYPE
	       && TREE_CODE (TREE_TYPE (rhs)) == INTEGER_TYPE)
	{
	  if (!lhs_cand_p && !integer_cst_p (rhs))
	    return false;
	  step = integer_one_node;
	}
      else
	return false;

      tree_code code = gimple_cond_code (cond);
      if (edge->flags & EDGE_TRUE_VALUE)
	code = opposite_cond_code (code);

      if (!fill_loop_var_range (loop, code, lhs, rhs, step))
	return false;

      if (iterate_var_p (loop, lhs))
	lhs = get_loop_var (loop, lhs);

      if (!fill_loop_var_range (loop, code, lhs, rhs, step))
	return false;
    }

  return true;
}

/* fill loop variable range according to loop exit's condition and step.  */

bool
array_dse_callee::fill_loop_var_range (loop_p loop, tree_code code,
				       tree lhs, tree rhs, tree step)
{
  for (auto bb : get_all_dominated_blocks (CDI_DOMINATORS, loop->header))
    {
      auto lhs_range = get_var_range (bb, lhs);
      auto rhs_range = get_var_range (bb, rhs);
      tree lhs_min = lhs_range.min ();
      tree lhs_max = lhs_range.max ();
      tree rhs_min = rhs_range.min ();
      tree rhs_max = rhs_range.max ();
      bool in_loop = flow_bb_inside_loop_p (loop, bb);

      switch (code)
	{
	  case LT_EXPR:
	    lhs_max = in_loop ? minus_tree (rhs_max, step) : rhs_max;
	    rhs_min = in_loop ? plus_tree (lhs_min, step) : lhs_min;
	    break;
	  case LE_EXPR:
	    lhs_max = in_loop ? rhs_max : plus_tree (rhs_max, step);
	    rhs_min = in_loop ? lhs_min : minus_tree (lhs_min, step);
	    break;
	  case GT_EXPR:
	    lhs_min = in_loop ? plus_tree (rhs_min, step) : rhs_min;
	    rhs_max = in_loop ? minus_tree (lhs_max, step) : lhs_max;
	    break;
	  case GE_EXPR:
	    lhs_min = in_loop ? rhs_min : minus_tree (rhs_min, step);
	    rhs_max = in_loop ? lhs_max : plus_tree (lhs_max, step);
	    break;
	  default:
	    return false;
	}

      if (loop_var_p (loop, lhs) || iterate_var_p (loop, lhs))
	var_range[lhs][bb] = value_range{lhs_min, lhs_max};
      if (loop_var_p (loop, rhs) || iterate_var_p (loop, rhs))
	var_range[rhs][bb] = value_range{rhs_min, rhs_max};

      if (integer_onep (step) && loop_var_p (loop, lhs)
	  && !fill_loop_ptr_range (loop, bb, minus_tree (lhs_max, lhs_min)))
	return false;
    }

  return true;
}

/* If the variable in loop exit's condition is a integer, like
     for (i = 0; i < n; i++)

   fill other pointers' range in the same loop.
 */

bool
array_dse_callee::fill_loop_ptr_range (loop_p loop, basic_block bb,
				       tree loop_length)
{
  if (!loop_length)
    return false;

  auto ret = compare_tree (bb, loop_length, integer_zero_node);
  if (!ret)
    return false;

  if (ret == LT)
    {
      unreachable_blocks.add (bb);
      return true;
    }

  tree length = mult_tree (bb, loop_length, elem_size);
  if (!length)
    return false;

  for (auto ptr : loop_ptrs[loop])
    {
      auto &range = var_range[ptr][bb];
      tree min = range.min ();
      tree max = range.max ();
      if (infinite_p (min) != infinite_kind::NON_INF)
	{
	  if (infinite_p (max) != infinite_kind::NON_INF)
	    return false;
	  min = minus_tree (max, length);
	}
      else if (infinite_p (max) != infinite_kind::NON_INF)
	{
	  if (infinite_p (min) != infinite_kind::NON_INF)
	    return false;
	  max = plus_tree (min, length);
	}
      else
	return false;

      range = value_range{min, max};
    }

  return true;
}

bool
array_dse_callee::loop_header_p (basic_block bb)
{
  return bb_loop_header_p (bb)
	 && (!main_loop || bb->loop_father != main_loop);
}

bool
array_dse_callee::iterate_var_p (loop_p loop, tree var)
{
  if (!var)
    return false;

  tree loop_var = get_loop_var (loop, var);
  return loop_var && loop_var != var;
}

/* Find the loop variable from the GIVEN var throught its def chain.  */

tree
array_dse_callee::get_loop_var (loop_p loop, tree var)
{
  if (TREE_CODE (var) != SSA_NAME || !SSA_NAME_VAR (var))
    return nullptr;

  tree result = nullptr;

  auto_bitmap visited;
  auto_vec<tree> worklist;
  worklist.safe_push (var);

  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();
      if (TREE_CODE (var) != SSA_NAME)
	return nullptr;

      if (!bitmap_set_bit (visited, SSA_NAME_VERSION (t)))
	continue;

      if (loop_var_p (loop, t) && SSA_NAME_VAR (t) == SSA_NAME_VAR (var))
	{
	  if (result && result != t)
	    return nullptr;
	  result = t;
	  continue;
	}

      gimple *stmt = SSA_NAME_DEF_STMT (t);
      basic_block bb = gimple_bb (stmt);
      if (!bb || !flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
	return nullptr;

      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
	    worklist.safe_push (gimple_phi_arg_def (stmt, i));
	  continue;
	}

      if (!is_gimple_assign (stmt)
	  || (gimple_assign_rhs_code (stmt) != POINTER_PLUS_EXPR
	      && gimple_assign_rhs_code (stmt) != PLUS_EXPR))
	return nullptr;

      worklist.safe_push (gimple_assign_rhs1 (stmt));
    }

  return result;
}

bool
array_dse_callee::find_var_range (tree var, basic_block bb)
{
  auto iter1 = var_range.find (var);
  if (iter1 == var_range.end ())
    return false;

  auto iter2 = iter1->second.find (bb);
  return iter2 != iter1->second.end ();
}

array_dse_edge::array_dse_edge (cgraph_edge *edge, array_dse_callee *callee)
  : call_edge (edge),
    callee (callee)
{
}

bool
array_dse_edge::analyze ()
{
  cfun_saver save (call_edge->caller, LOOPS_NORMAL);

  if (gimple_call_num_args (call_edge->call_stmt) != callee->PARAM_NUM)
    return false;

  return find_local_array_from_arg () && check_array_usage ()
	 && check_len_arg_range ();
}

bool
array_dse_edge::fully_redundant ()
{
  return array_arg_start > read_upper_bound;
}

tree
array_dse_edge::get_bound_addr ()
{
  unsigned HOST_WIDE_INT bound_size = (read_upper_bound + 1) * elem_size;
  tree bound_size_expr = build_int_cst (size_type_node, bound_size);

  tree addr_type = build_pointer_type (TREE_TYPE (array));
  tree array_addr = build1 (ADDR_EXPR, addr_type, array);

  return build2 (POINTER_PLUS_EXPR, addr_type, array_addr, bound_size_expr);
}

/* Find the local array used by call argument.  */

bool
array_dse_edge::find_local_array_from_arg ()
{
  tree arg = gimple_call_arg (call_edge->call_stmt, callee->array_param_index);

  while (TREE_CODE (arg) == ADDR_EXPR || TREE_CODE (arg) == MEM_REF)
    arg = TREE_OPERAND (arg, 0);

  if (!arg || !VAR_P (arg) || TREE_CODE (TREE_TYPE (arg)) != ARRAY_TYPE
      || decl_function_context (arg) != current_function_decl)
    return false;

  array = arg;
  elem_size = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (array))));
  array_size = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (array))) / elem_size;

  return true;
}

bool
array_dse_edge::check_array_usage ()
{
  if (!collect_array_accesses ())
    return false;

  if (!find_inner_array ())
    return false;

  for (auto [var, stmt] : array_accesses)
    if (!check_access_kind (stmt, var))
      return false;

  collect_call_block_succs ();
  if (!calc_read_bound () || !calc_array_arg_start ())
    return false;

  if (call_block_succs.contains (gimple_bb (call_edge->call_stmt))
      && !check_optimized_area_rewrite ())
    return false;

  return true;
}

bool
array_dse_edge::collect_array_accesses ()
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (bb->flags & BB_IRREDUCIBLE_LOOP)
	return false;

      for (auto gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = as_a<gphi *> (gsi_stmt (gsi));
	  tree result = gimple_phi_result (phi);
	  if (walk_tree (&result, sub_expr_p, array, nullptr))
	    if (!check_array_access (phi, result))
	      return false;

	  for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      if (!walk_tree (&arg, sub_expr_p, array, nullptr))
		continue;

	      if (!check_array_access (phi, arg))
		return false;
	    }
	}

      for (auto gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (gimple_clobber_p (stmt) || call_stmt_p (stmt))
	    continue;

	  for (unsigned i = 0; i < gimple_num_ops (stmt); i++)
	    {
	      tree var = gimple_op (stmt, i);
	      if (!var)
		continue;

	      if (!walk_tree (&var, sub_expr_p, array, nullptr))
		continue;

	      if (!is_gimple_assign (stmt))
		return false;

	      if (!check_array_access (stmt, var))
		return false;
	    }
	}

    }

  renumber_gimple_stmt_uids (cfun);

  return !array_accesses.is_empty ();
}

bool
array_dse_edge::check_array_access (gimple *stmt, tree var)
{
  if (array_ref_p (var))
    return gimple_assign_single_p (stmt) && !array_accesses.put (var, stmt);

  if (array_addr_p (var))
    return check_array_address (stmt, var);

  return false;
}

bool
array_dse_edge::check_array_address (gimple *stmt, tree addr)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    return check_array_address (as_a<gphi *> (stmt), addr);

  if (is_gimple_assign (stmt))
    return check_array_address (as_a<gassign *> (stmt), addr);

  return false;
}

bool
array_dse_edge::check_array_address (gphi *phi, tree addr)
{
  tree result = gimple_phi_result (phi);
  if (TREE_CODE (result) != SSA_NAME)
    return false;

  if (array_address_vars.contains (result))
    return true;

  for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);
      if (arg == addr)
	continue;

      if (TREE_CODE (arg) != SSA_NAME)
	return false;

      /* Only support simple loop variable: VAR is the initial address of
	 phi RESULT and other ARG must be defined by RESULT + offset.  */
      gimple *stmt = SSA_NAME_DEF_STMT (arg);
      if (!is_gimple_assign (stmt)
	  || gimple_assign_rhs_code (stmt) != POINTER_PLUS_EXPR
	  || gimple_assign_rhs1 (stmt) != result)
	return false;
    }

  array_address_vars.add (result);

  return check_access_from_address (result);
}

bool
array_dse_edge::check_array_address (gassign *assign, tree addr)
{
  if (!gimple_assign_single_p (assign)
      && gimple_assign_rhs_code (assign) != POINTER_PLUS_EXPR
      && gimple_assign_rhs1 (assign) != addr)
    return false;

  tree lhs = gimple_assign_lhs (assign);
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  array_address_vars.add (lhs);

  return check_access_from_address (lhs);
}

bool
array_dse_edge::check_access_from_address (tree addr)
{
  gimple *stmt;
  imm_use_iterator iter;
  FOR_EACH_IMM_USE_STMT (stmt, iter, addr)
    {
      for (unsigned i = 0; i < gimple_num_ops (stmt); i++)
	{
	  tree op = gimple_op (stmt, i);
	  if (walk_tree (&op, sub_expr_p, addr, nullptr)
	      && !check_array_access (stmt, op))
	    return false;
	}
    }

  return true;
}

bool
array_dse_edge::check_access_kind (gimple *stmt, tree var)
{
  gcc_assert (gimple_assign_single_p (stmt));

  auto &kind = access_kinds.get_or_insert (var);

  tree lhs = gimple_assign_lhs (stmt);
  if (var == lhs)
    {
      kind = WRITE;
      return true;
    }

  gcc_assert (var == gimple_assign_rhs1 (stmt));

  if (!inner_array)
    {
      kind = READ;
      return true;
    }

  auto_bitmap visited;
  kind = check_access_kind_iterate (lhs, visited);
  return kind != ACCESS_ERROR;
}

access_kind
array_dse_edge::check_access_kind_iterate (tree var, auto_bitmap &visited)
{
  if (!var || TREE_CODE (var) != SSA_NAME)
    return ACCESS_ERROR;

  if (!bitmap_set_bit (visited, SSA_NAME_VERSION (var)))
    return NONE;

  int kind = NONE;

  imm_use_iterator iter;
  gimple *stmt = nullptr;
  FOR_EACH_IMM_USE_STMT (stmt, iter, var)
    {
      if (walk_stmt_load_store_ops (stmt, var, find_base, nullptr))
	kind |= READ;

      if (walk_stmt_load_store_ops (stmt, var, nullptr, find_base))
	kind |= WRITE;

      if (kind)
	continue;

      tree next_var = nullptr;
      if (is_gimple_assign (stmt))
	{
	  if ((!gimple_assign_single_p (stmt) && !gimple_assign_cast_p (stmt))
	      || gimple_assign_rhs1 (stmt) != var)
	    return ACCESS_ERROR;

	  tree lhs = gimple_assign_lhs (stmt);
	  if (array_ref_p (lhs))
	    {
	      kind |= READ;
	      continue;
	    }

	  next_var = lhs;
	}
      else if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  if (gimple_phi_arg_p (stmt, var))
	    next_var = gimple_phi_result (stmt);
	}
      else if (gimple_code (stmt) == GIMPLE_COND)
	{
	  if (gimple_cond_lhs (stmt) == var || gimple_cond_rhs (stmt) == var)
	    {
	      kind |= READ;
	      continue;
	    }
	}

      access_kind next_kind = check_access_kind_iterate (next_var, visited);
      if (next_kind == ACCESS_ERROR)
	return ACCESS_ERROR;
	
      kind |= next_kind;
    }

  return static_cast<access_kind> (kind);
}

bool
array_dse_edge::find_inner_array ()
{
  tree type = TREE_TYPE (array);
  unsigned ptr_layers = get_ptr_layers (type);
  gcc_assert (ptr_layers);

  /* No inner source array.  */
  if (ptr_layers == 1)
    {
      inner_elem_type = TREE_TYPE (array);
      return true;
    }

  /* It's hard to trace all source of array.  */
  if (ptr_layers > 2
      || TREE_CODE (TREE_TYPE (TREE_TYPE (type))) != RECORD_TYPE)
    return false;

  inner_elem_type = TREE_TYPE (TREE_TYPE (type));

  for (auto [var, stmt] : array_accesses)
    {
      tree lhs = gimple_get_lhs (stmt);
      if (lhs != var)
	continue;

      if (!array_ref_p (lhs) || !is_gimple_assign (stmt))
	return false;

      tree rhs = gimple_assign_rhs1 (stmt);
      if (array_ref_p (rhs))
	continue;

      if (TREE_CODE (rhs) != SSA_NAME)
	return false;

      tree base = rhs;
      gimple *def_stmt = SSA_NAME_DEF_STMT (base);
      while (is_gimple_assign (def_stmt)
	     && gimple_assign_rhs_code (def_stmt) == POINTER_PLUS_EXPR)
	{
	  base = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (base) != SSA_NAME)
	    return false;
	  def_stmt = SSA_NAME_DEF_STMT (base);
	}

      if (!gimple_call_builtin_p (def_stmt, BUILT_IN_CALLOC))
	return false;

      /* Only support unique source. The inner_array must be used only once,
	 assigned its address to the candidate array.  */
      if (inner_array)
	return false;

      /* array: T *[], base: T *.  */
      if (TREE_TYPE (TREE_TYPE (array)) != TREE_TYPE (base))
	return false;

      if (!unique_use_p (base, stmt) || !initialize_assign_p (stmt))
	return false;

      inner_array = base;
    }

  return true;
}

bool
array_dse_edge::unique_use_p (tree var, gimple *unique_assign) const
{
  auto_vec<tree> worklist;
  auto_bitmap visited;
  worklist.safe_push (var);

  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();
      if (TREE_CODE (t) != SSA_NAME)
	return false;

      if (!bitmap_set_bit (visited, SSA_NAME_VERSION (t)))
	continue;

      imm_use_iterator iter;
      gimple *stmt = nullptr;
      FOR_EACH_IMM_USE_STMT (stmt, iter, t)
	{
	  if (gimple_call_builtin_p (stmt, BUILT_IN_FREE))
	    continue;

	  if (!is_gimple_assign (stmt))
	    return false;

	  if (stmt == unique_assign)
	    continue;

	  worklist.safe_push (gimple_assign_lhs (stmt));
	}
    }

  return true;
}

bool
array_dse_edge::initialize_assign_p (gimple *stmt) const
{
  if (!stmt || !gimple_bb (stmt))
    return false;

  hash_set<basic_block> preds;
  auto_vec<basic_block> worklist;
  worklist.safe_push (gimple_bb (stmt));

  while (!worklist.is_empty ())
    {
      basic_block bb = worklist.pop ();
      if (preds.add (bb))
	continue;

      for (auto e : bb->preds)
	worklist.safe_push (e->src);
    }

  for (auto [var, access_stmt] : array_accesses)
    {
      if (access_stmt == stmt)
	continue;

      if (preds.contains (gimple_bb (access_stmt)))
	return false;
    }

  return true;
}

bool
array_dse_edge::calc_read_bound ()
{
  for (auto [var, stmt] : array_accesses)
    {
      if (!after_call_stmt_p (stmt) || !read_array_p (var))
	continue;

      auto range = calc_ref_range (var);
      if (!integer_cst_p (range.max ()))
	return false;

      auto max = tree_to_shwi (range.max ());
      if (max % elem_size)
	return false;

      if (max / elem_size > read_upper_bound)
	read_upper_bound = max / elem_size;
    }

  return true;
}

value_range
array_dse_edge::calc_ref_range (tree var)
{
  tree_code code = TREE_CODE (var);
  /* Array_ref's second op is an index.  Convert it to address offset.  */
  if (code == ARRAY_REF)
    {
      auto r = calc_offset_range (TREE_OPERAND (var, 1));
      if (r.varying_p ())
	return r;

      gcc_assert (integer_cst_p (r.min ()) && integer_cst_p (r.max ()));
      return make_range (tree_to_shwi (r.min ()) * elem_size,
			 tree_to_shwi (r.max ()) * elem_size);
    }

  gcc_assert (code == MEM_REF);
  auto r1 = calc_addr_range (TREE_OPERAND (var, 0));
  auto r2 = calc_offset_range (TREE_OPERAND (var, 1));

  return value_range{plus_tree (r1.min (), r2.min ()),
		     plus_tree (r1.max (), r2.max ())};
}

value_range
array_dse_edge::calc_addr_range (tree var)
{
  if (array_address_vars.contains (var))
    {
      gcc_assert (TREE_CODE (var) == SSA_NAME);
      gimple *stmt = SSA_NAME_DEF_STMT (var);
      if (is_gimple_assign (stmt))
	{
	  auto r1 = calc_addr_range (gimple_assign_rhs1 (stmt));
	  auto r2 = calc_offset_range (gimple_assign_rhs2 (stmt));
	  return value_range{plus_tree (r1.min (), r2.min ()),
			     plus_tree (r1.max (), r2.max ())};
	}

      return calc_simple_loop_range (var);
    }

  if (TREE_CODE (var) != ADDR_EXPR)
    return value_range{RANGE_TYPE};

  tree op = TREE_OPERAND (var, 0);
  if (op == array)
    return make_range (0);

  if (!array_ref_p (op))
    return value_range{RANGE_TYPE};

  return calc_ref_range (op);
}

value_range
array_dse_edge::calc_offset_range (tree offset)
{
  tree var = strip_ssa_copy (offset);
  if (integer_cst_p (var))
    return make_range (tree_to_shwi (var));

  if (TREE_CODE (var) != SSA_NAME)
    return value_range{RANGE_TYPE};

  gimple *stmt = SSA_NAME_DEF_STMT (var);
  if (gimple_code (stmt) == GIMPLE_PHI)
    return calc_simple_loop_range (var);

  if (!is_gimple_assign (stmt) || gimple_assign_rhs_code (stmt) != MULT_EXPR
      || !integer_cst_p (gimple_assign_rhs2 (stmt))
      || TREE_INT_CST_LOW (gimple_assign_rhs2 (stmt)) != elem_size)
    return value_range{RANGE_TYPE};

  auto range = calc_offset_range (gimple_assign_rhs1 (stmt));
  if (!integer_cst_p (range.min ()) || !integer_cst_p (range.max ()))
    return value_range{RANGE_TYPE};

  return make_range (tree_to_shwi (range.min ()) * elem_size,
		     tree_to_shwi (range.max ()) * elem_size);
}

value_range
array_dse_edge::calc_simple_loop_range (tree var)
{
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  basic_block bb = gimple_bb (stmt);
  loop_p loop = bb->loop_father;

  if (!loop || loop->header != bb || !loop->any_upper_bound)
    return value_range{RANGE_TYPE};

  tree init_var = PHI_ARG_DEF_FROM_EDGE (stmt, loop_preheader_edge (loop));
  tree iterate_var = PHI_ARG_DEF_FROM_EDGE (stmt, loop_latch_edge (loop));

  value_range init_range;
  if (array_addr_p (init_var))
    init_range = calc_addr_range (init_var);
  else
    init_range = calc_offset_range (init_var);

  if (!init_range.singleton_p () || !integer_cst_p (init_range.min ()))
    return value_range{RANGE_TYPE};

  HOST_WIDE_INT init_value = tree_to_shwi (init_range.min ());
  int step = calc_loop_var_step (var, iterate_var);
  int upper_bound = loop->nb_iterations_upper_bound.to_shwi ();

  return make_range (init_value, init_value + step * upper_bound);
}

void
array_dse_edge::collect_call_block_succs ()
{
  basic_block call_block = gimple_bb (call_edge->call_stmt);
  auto_vec<basic_block> worklist;
  for (auto e : call_block->succs)
    worklist.safe_push (e->dest);

  while (!worklist.is_empty ())
    {
      basic_block bb = worklist.pop ();
      if (call_block_succs.add (bb))
	continue;

      for (auto e : bb->succs)
	worklist.safe_push (e->dest);
    }
}

bool
array_dse_edge::calc_array_arg_start ()
{
  tree array_arg = gimple_call_arg (call_edge->call_stmt,
				    callee->array_param_index);
  if (!array_addr_p (array_arg))
    return false;

  auto range = calc_addr_range (array_arg);
  if (!range.singleton_p () || !integer_cst_p (range.min ()))
    return false;

  auto value = tree_to_shwi (range.min ());
  if (value % elem_size)
    return false;

  array_arg_start = value / elem_size;
  return true;
}

bool
array_dse_edge::check_optimized_area_rewrite ()
{
  tree arg = gimple_call_arg (call_edge->call_stmt, callee->len_param_index);
  if (!arg)
    return false;

  tree var = strip_ssa_copy (arg);
  if (!var || TREE_CODE (var) != SSA_NAME)
    return false;

  gimple *stmt = SSA_NAME_DEF_STMT (var);
  loop_p loop = gimple_bb (stmt)->loop_father;
  if (!loop || !loop_var_p (loop, var))
    return false;

  /* To make sure the optimized area of array is fully rewritten, the loop
     step must be 1.  We only support one iterate stmt now.  */
  tree iterate_var = PHI_ARG_DEF_FROM_EDGE (stmt, loop_latch_edge (loop));
  hash_set<gimple *> iterate_stmts;
  if (calc_loop_var_step (var, iterate_var, &iterate_stmts) != 1
      || iterate_stmts.elements () != 1)
    return false;

  gimple *iter_stmt = *iterate_stmts.begin ();
  if (gimple_assign_rhs1 (iter_stmt) != var)
    return false;

  /* Check if the element has been fully written.  */
  basic_block bb = gimple_bb (iter_stmt);
  if (!check_full_write_elem (bb, gimple_assign_lhs (iter_stmt)))
    return false;

  /* Check if the start address is less equal than the read_upper_bound.  */
  tree init_var = PHI_ARG_DEF_FROM_EDGE (stmt, loop_preheader_edge (loop));
  init_var = strip_init_var (init_var, var);
  if (!init_var)
    return false;

  auto range = calc_offset_range (init_var);
  if (!integer_cst_p (range.min ()) || !integer_cst_p (range.max ()))
    return false;

  len_arg_min = tree_to_shwi (range.min ());

  return tree_to_shwi (range.max ()) + array_arg_start <= read_upper_bound;
}

bool
array_dse_edge::check_full_write_elem (basic_block bb, tree index)
{
  hash_set<tree> visited_fields;

  for (auto [var, stmt] : array_accesses)
    {
      /* Must in the same block.  */
      if (gimple_bb (stmt) != bb)
	continue;

      if (!write_array_p (var))
	continue;

      if (TREE_CODE (var) != MEM_REF
	  || !array_index_of_addr_p (index, TREE_OPERAND (var, 0))
	  || !integer_zerop (TREE_OPERAND (var, 1)))
	continue;

      /* Directly write to array.  */
      tree lhs = gimple_assign_lhs (stmt);
      if (var == lhs)
	return true;
      else if (!inner_array)
	continue;

      imm_use_iterator iter;
      gimple *use_stmt = nullptr;
      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	{
	  tree ref = gimple_get_lhs (use_stmt);
	  if (!ref || TREE_CODE (ref) != COMPONENT_REF)
	    continue;

	  visited_fields.add (TREE_OPERAND (ref, 1));
	}
    }

  return inner_array
	 && visited_fields.elements () == fields_length (inner_elem_type);
}

bool
array_dse_edge::array_index_of_addr_p (tree index, tree addr)
{
  if (TREE_CODE (index) != SSA_NAME || TREE_CODE (addr) != SSA_NAME)
    return false;

  /* Check pattern: addr = &array + offset.  */
  gimple *stmt = SSA_NAME_DEF_STMT (addr);
  if (!is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != POINTER_PLUS_EXPR)
    return false;

  tree rhs1 = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (rhs1) != ADDR_EXPR || TREE_OPERAND (rhs1, 0) != array)
    return false;

  tree rhs2 = gimple_assign_rhs2 (stmt);
  if (TREE_CODE (rhs2) != SSA_NAME)
    return false;

  /* Check pattern: offset = index * elem_size.  */
  stmt = SSA_NAME_DEF_STMT (rhs2);
  if (!is_gimple_assign (stmt) || gimple_assign_rhs_code (stmt) != MULT_EXPR)
    return false;

  rhs1 = gimple_assign_rhs1 (stmt);
  rhs2 = gimple_assign_rhs2 (stmt);

  return strip_ssa_copy (rhs1) == index && integer_cst_p (rhs2)
	 && TREE_INT_CST_LOW (rhs2) == elem_size;
}

tree
array_dse_edge::strip_init_var (tree init_var, tree var)
{
  tree last = var;
  while (true)
    {
      if (TREE_CODE (init_var) != SSA_NAME)
	break;

      gimple *stmt = SSA_NAME_DEF_STMT (init_var);
      loop_p loop = gimple_bb (stmt)->loop_father;
      if (!loop || !loop_var_p (loop, init_var))
	break;

      auto latch_edge = loop_latch_edge (loop);
      auto preheader_edge = loop_preheader_edge (loop);
      if (!latch_edge || !preheader_edge
	  || PHI_ARG_DEF_FROM_EDGE (stmt, latch_edge) != last)
	break;

      last = init_var;
      init_var = PHI_ARG_DEF_FROM_EDGE (stmt, preheader_edge);
    }

  return strip_ssa_copy (init_var);
}

bool
array_dse_edge::check_len_arg_range ()
{
  return check_len_arg_lower_bound ()
	 && check_len_arg_upper_bound ();
}

/* Check: ARG >= 1 (assumption in callee analysis).  */

bool
array_dse_edge::check_len_arg_lower_bound ()
{
  if (len_arg_min >= 1)
    return true;

  /* If the len_arg_min recorded previous doesn't meet the condition, try to
     update it by checking condition jump.  */
  tree arg = gimple_call_arg (call_edge->call_stmt, callee->len_param_index);
  if (!arg)
    return false;

  tree var = strip_ssa_copy (arg);
  if (!var || TREE_CODE (var) != SSA_NAME)
    return false;

  basic_block call_block = gimple_bb (call_edge->call_stmt);
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple *stmt = last_stmt (bb);
      if (!stmt || gimple_code (stmt) != GIMPLE_COND)
	continue;

      tree_code code = gimple_cond_code (stmt);
      tree lhs = gimple_cond_lhs (stmt);
      tree rhs = gimple_cond_rhs (stmt);

      if (lhs != var || !integer_cst_p (rhs))
	continue;

      edge true_edge;
      edge false_edge;
      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);
      if (!true_edge || !false_edge)
	continue;

      if (dominated_by_p (CDI_DOMINATORS, call_block, false_edge->dest))
	code = opposite_cond_code (code);
      else if (!dominated_by_p (CDI_DOMINATORS, call_block, true_edge->dest))
	continue;

      HOST_WIDE_INT rvalue = TREE_INT_CST_LOW (rhs);
      switch (code)
	{
	  case GT_EXPR:
	    len_arg_min = std::max (len_arg_min, rvalue + 1);
	    break;
	  case GE_EXPR:
	    [[fallthrough]];
	  case EQ_EXPR:
	    len_arg_min = std::max (len_arg_min, rvalue);
	    break;
	  case NE_EXPR:
	    if (len_arg_min == rvalue)
	      len_arg_min++;
	    break;
	  default:
	    break;
	}
    }

  return len_arg_min >= 1;
}

/* We can assume that the array will not be accessed out of bounds.
   So we use array_size as the upper bound of len arg.  */

bool
array_dse_edge::check_len_arg_upper_bound ()
{
  return array_size <= callee->get_len_param_max ();
}

bool
array_dse_edge::after_call_stmt_p (gimple *stmt)
{
  if (call_stmt_p (stmt))
    return false;

  basic_block bb = gimple_bb (stmt);
  if (bb == gimple_bb (call_edge->call_stmt)
      && gimple_uid (stmt) > gimple_uid (call_edge->call_stmt))
    return true;

  return call_block_succs.contains (bb);
}

bool
array_dse_edge::write_array_p (tree var)
{
  auto *kind = access_kinds.get (var);
  if (!kind)
    return false;

  return *kind & WRITE;
}

bool
array_dse_edge::read_array_p (tree var)
{
  auto *kind = access_kinds.get (var);
  if (!kind)
    return false;

  return *kind & READ;
}

bool
array_dse_edge::call_stmt_p (gimple *stmt) const
{
  return stmt == call_edge->call_stmt;
}

bool
array_dse_edge::array_ref_p (tree var)
{
  if (!var)
    return false;

  if (TREE_CODE (var) == ARRAY_REF)
    return TREE_OPERAND (var, 0) == array;

  return (TREE_CODE (var) == MEM_REF && array_addr_p (TREE_OPERAND (var, 0)));
}

bool
array_dse_edge::array_addr_p (tree var)
{
  if (array_address_vars.contains (var))
    return true;

  if (TREE_CODE (var) != ADDR_EXPR)
    return false;

  tree op = TREE_OPERAND (var, 0);
  return op == array || array_ref_p (op);
}

unsigned
ipa_array_dse::execute ()
{
  cgraph_node *node;
  FOR_EACH_FUNCTION (node)
    {
      if (!node->real_symbol_p () || !node->definition
	  || !node->has_gimple_body_p () || node->inlined_to)
	continue;
      node->get_body ();

      if (!DECL_STRUCT_FUNCTION (node->decl))
	continue;

      nodes.safe_push (node);
    }

  if (!find_array_dse_candidate_callees ())
    {
      if (dump_file)
	fprintf (dump_file, "Fail finding array dse candidate callees\n");
      return 0;
    }

  if (!find_array_dse_candidate_edges ())
    {
      if (dump_file)
	fprintf (dump_file, "Fail finding array dse candidate edges\n");
      return 0;
    }

  for (auto edge : candidate_edges)
    apply_array_dse (edge);

  symtab->remove_unreachable_nodes (dump_file);

  return TODO_update_ssa;
}

bool
ipa_array_dse::find_array_dse_candidate_callees ()
{
  if (dump_file)
    fprintf (dump_file, "Finding array dse candidate callees\n\n");

  for (auto node : nodes)
    {
      if (!tree_versionable_function_p (node->decl)
	  || !opt_for_fn (node->decl, optimize))
	continue;

      const char *fn_name = node->dump_asm_name ();
      if (dump_file)
	fprintf (dump_file, "Analyzing callee: %s\n", fn_name);

      auto *callee = new array_dse_callee (node);
      if (!callee->analyze ())
	{
	  delete callee;
	  continue;
	}

      if (dump_file)
	{
	  fprintf (dump_file, "Found candidate callee: %s\n", fn_name);
	  if (dump_flags & TDF_DETAILS)
	    dump_function_to_file (node->decl, dump_file, dump_flags);
	  fprintf (dump_file, "\n");
	}

      candidate_callees.safe_push (callee);
    }

  return !candidate_callees.is_empty ();
}

bool
ipa_array_dse::find_array_dse_candidate_edges ()
{
  if (dump_file)
    fprintf (dump_file, "Finding array dse candidate call edges\n\n");

  for (auto *callee : candidate_callees)
    {
      cgraph_edge *e = callee->node->callers;
      while (e && e->caller == callee->node)
	e = e->next_caller;

      for (auto *c : candidate_callees)
	if (e->caller == c->node)
	  return false;

      auto *edge = new array_dse_edge (e, callee);
      if (!edge->analyze ())
	{
	  delete edge;
	  continue;
	}

      if (dump_file)
	{
	  fprintf (dump_file, "Found candidate call edge: ");
	  print_gimple_stmt (dump_file, e->call_stmt, 0, TDF_NONE);
	  if (dump_flags & TDF_DETAILS)
	    dump_function_to_file (e->caller->decl, dump_file, dump_flags);
	  fprintf (dump_file, "\n");
	}

      candidate_edges.safe_push (edge);
    }

  return !candidate_edges.is_empty ();
}

bool
ipa_array_dse::apply_array_dse (array_dse_edge *ad_edge)
{
  /* Remove call stmt if it's fully redundant.  */
  if (ad_edge->fully_redundant ())
    {
      cgraph_node *caller = ad_edge->call_edge->caller;
      gimple *call_stmt = ad_edge->call_edge->call_stmt;

      cfun_saver save (caller);

      auto gsi = gsi_for_stmt (call_stmt);
      basic_block call_bb = gimple_bb (call_stmt);
      tree fndecl = gimple_call_fndecl (call_stmt);
      caller->remove_stmt_references (call_stmt);
      unlink_stmt_vdef (call_stmt);
      if (gsi_remove (&gsi, true))
	gimple_purge_dead_eh_edges (call_bb);
      cgraph_update_edges_for_call_stmt (call_stmt, fndecl, nullptr);

      return true;
    }

  /* Insert array redundant bound check to callee.  */
  array_dse_callee *callee = ad_edge->callee;
  cgraph_node *orig_callee = ad_edge->callee->node;
  cgraph_node *new_callee
    = orig_callee->create_version_clone_with_body (vNULL, NULL, NULL, NULL,
						   NULL, "array_dse", NULL);

  if (!transform_new_callee (callee, new_callee))
    return false;

  tree bound_addr = ad_edge->get_bound_addr ();
  rewrite_call_edge (ad_edge->call_edge, new_callee, bound_addr);

  return true;
}

tree
ipa_array_dse::add_bound_param (tree param)
{
  vec<ipa_adjusted_param, va_gc> *new_params = NULL;
  auto_vec<tree> arg_decls;

  push_function_arg_decls (&arg_decls, cfun->decl);
  gcc_checking_assert (!arg_decls.is_empty ());
  vec_safe_reserve (new_params, arg_decls.length () + 1);

  for (unsigned i = 0; i < arg_decls.length (); ++i)
    {
      ipa_adjusted_param adj;

      memset (&adj, 0, sizeof (adj));

      adj.type = TREE_TYPE (arg_decls[i]);
      adj.base_index = i;
      adj.prev_clone_index = i;
      adj.op = IPA_PARAM_OP_COPY;
      new_params->quick_push (adj);
    }

  tree param_name = DECL_NAME (param);
  const char *name = concat (IDENTIFIER_POINTER (param_name), ".bound", NULL);
  ipa_adjusted_param adj;
  adj.type = TREE_TYPE (param);
  adj.base_index = arg_decls.length ();
  adj.prev_clone_index = arg_decls.length ();
  adj.op = IPA_PARAM_OP_NEW;
  new_params->quick_push (adj);

  auto adjustments = new ipa_param_body_adjustments (new_params, cfun->decl);
  adjustments->modify_formal_parameters ();
  delete adjustments;

  arg_decls.truncate (0);
  push_function_arg_decls (&arg_decls, cfun->decl);

  tree new_param = arg_decls.last ();
  DECL_NAME (new_param) = get_identifier (name);

  return get_or_create_ssa_default_def (cfun, new_param);
}

tree
ipa_array_dse::find_array_main_var (array_dse_callee *callee)
{
  int i = 0;
  tree param = DECL_ARGUMENTS (cfun->decl);
  while (i++ < callee->array_param_index)
    param = DECL_CHAIN (param);

  tree name;
  FOR_EACH_SSA_NAME (i, name, cfun)
    {
      if (!SSA_NAME_IS_DEFAULT_DEF (name)
	  || SSA_NAME_VAR (name) != param)
	continue;

      if (!callee->main_loop)
	return name;

      use_operand_p use_p;
      gimple *stmt;
      if (!single_imm_use (name, &use_p, &stmt)
	  || gimple_code (stmt) != GIMPLE_PHI)
	return nullptr;

      return gimple_phi_result (stmt);
    }

  return nullptr;
}

bool
ipa_array_dse::transform_new_callee (array_dse_callee *callee,
				     cgraph_node *new_node)
{
  cfun_saver save (new_node);

  tree bound_ssa = add_bound_param (callee->array_param);
  tree array = find_array_main_var (callee);
  if (!array)
    return false;

  edge e;
  if (callee->main_loop)
    {
      gimple *array_def_stmt = SSA_NAME_DEF_STMT (array);
      basic_block array_def_bb = gimple_bb (array_def_stmt);
      gcc_assert (gimple_code (array_def_stmt) == GIMPLE_PHI);
      e = split_block_after_labels (array_def_bb);
    }
  else
    {
      basic_block entry_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
      gcc_assert (single_succ_p (entry_bb));
      basic_block bb = split_edge (single_succ_edge (entry_bb));
      e = single_succ_edge (bb);
    }

  auto gsi = gsi_last_bb (e->src);
  gimple *cond = gimple_build_cond (GE_EXPR, array, bound_ssa, nullptr,
				    nullptr);
  gsi_insert_after (&gsi, cond, GSI_NEW_STMT);

  edge return_edge = make_edge (e->src, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
  basic_block return_bb = split_edge (return_edge);
  auto return_gsi = gsi_last_bb (return_bb);
  gsi_insert_after (&return_gsi, gimple_build_return (nullptr), GSI_NEW_STMT);

  e->flags &= ~EDGE_FALLTHRU;
  e->flags |= EDGE_FALSE_VALUE;
  single_pred_edge (return_bb)->flags |= EDGE_TRUE_VALUE;
  single_succ_edge (return_bb)->flags = 0;

  for (auto call_edge = new_node->callees; call_edge;
       call_edge = call_edge->next_callee)
    rewrite_call_edge (call_edge, new_node, bound_ssa);

  return true;
}

void
ipa_array_dse::rewrite_call_edge (cgraph_edge *edge, cgraph_node *new_node,
				  tree bound_addr)
{
  auto_vec<tree> args;
  gcall *call_stmt = edge->call_stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (call_stmt);
  cgraph_node *caller = edge->caller;
  cfun_saver save (caller);

  for (unsigned i = 0; i < gimple_call_num_args (call_stmt); i++)
    args.safe_push (gimple_call_arg (call_stmt, i));

  bound_addr = force_gimple_operand_gsi (&gsi, bound_addr, true,
					 NULL_TREE, true,
					 GSI_SAME_STMT);
  args.safe_push (bound_addr);

  gcall *new_call = gimple_build_call_vec (new_node->decl, args);

  if (tree vdef = gimple_vdef (call_stmt))
    {
      gimple_set_vdef (new_call, vdef);
      SSA_NAME_DEF_STMT (vdef) = new_call;
    }

  gimple_set_vuse (new_call, gimple_vuse (call_stmt));
  gimple_call_copy_flags (new_call, call_stmt);
  gimple_call_set_chain (new_call, gimple_call_chain (call_stmt));
  gsi_replace (&gsi, new_call, false);

  cgraph_update_edges_for_call_stmt (call_stmt,
				     gimple_call_fndecl (call_stmt), new_call);

  caller->remove_stmt_references (call_stmt);
  caller->record_stmt_references (new_call);
}

} // namespace array_dse

namespace {

const pass_data pass_data_ipa_array_dse =
{
  SIMPLE_IPA_PASS, /* type */
  "array-dse", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_ARRAY_DSE, /* tv_id */
  PROP_cfg | PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_array_dse : public simple_ipa_opt_pass
{
public:
  pass_ipa_array_dse (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_array_dse, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return optimize >= 3 && flag_ipa_array_dse;
    }

  virtual unsigned int execute (function *)
    {
      return array_dse::ipa_array_dse ().execute ();
    }

}; // class pass_ipa_array_dse

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_array_dse (gcc::context *ctxt)
{
  return new pass_ipa_array_dse (ctxt);
}
