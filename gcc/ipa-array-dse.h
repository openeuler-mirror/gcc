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

#ifndef IPA_ARRAY_DSE_H
#define IPA_ARRAY_DSE_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "function.h"
#include "cfgloop.h"
#include "hash-map.h"
#include "tree-core.h"
#include "tree.h"
#include "bitmap.h"
#include "value-range.h"
#include <map>

namespace array_dse {

enum compare_result
{
  COMPARE_ERROR = 0,
  LT = 1,
  EQ = 2,
  GT = 4,
  LE = LT | EQ,
  GE = GT | EQ,
  NE = LT | GT
};

enum access_kind
{
  NONE = 0,
  READ = 1,
  WRITE = 2,
  ACCESS_ERROR = 4
};

enum class infinite_kind
{
  NON_INF,
  INF,
  NINF
};

/* Address analyzer.  */
class addr_analyzer
{
public:
  tree get_address (tree var);

private:
  tree analyze_address (tree var);

private:
  hash_map<tree, tree> address_map;
};

class array_dse_callee
{
public:
  array_dse_callee (cgraph_node *node);

  bool analyze ();
  unsigned HOST_WIDE_INT get_len_param_max () const;

private:
  tree mult_tree (basic_block bb, tree t1, tree t2);
  tree div_tree (basic_block bb, tree t1, tree t2);
  tree lshift_tree (tree t1, tree t2);
  tree rshift_tree (tree t1, tree t2);
  tree max_tree (basic_block bb, tree t1, tree t2);
  tree min_tree (basic_block bb, tree t1, tree t2);
  HOST_WIDE_INT calc_tree_value (tree t, HOST_WIDE_INT n_value);
  value_range calc_tree_range (basic_block bb, tree t);
  bool get_factor (tree t, double &factor);
  bool positive_factor_p (tree t);
  value_range build_range (basic_block bb, tree_code op,
			   const value_range &r1, const value_range &r2);
  compare_result compare_tree (basic_block bb, tree t1, tree t2);
  compare_result compare_tree_by_minus (basic_block bb, tree t1, tree t2);
  bool filter_function () const;
  bool no_return_p () const;
  bool find_main_vars ();
  void find_tail_recursive_loop (tree *default_def);
  bool find_candidate_array ();
  bool find_length_param ();
  void collect_read_write_ptrs ();
  void calc_length_param_max ();
  bool check_pointer (tree var, tree &len, unsigned &size);
  bool check_offset (tree var, tree &len, unsigned &size,
		     auto_vec<tree> &worklist);
  bool check_mult_expr (gimple *stmt, tree &len, unsigned &size);
  int find_param_index (tree base);
  bool check_array_usage ();
  void calc_len_range ();
  void update_len_range (basic_block start, const value_range &new_range);
  value_range invert_range (const value_range &range) const;
  value_range get_range_from_cond (gimple *stmt);
  value_range get_var_range (basic_block bb, tree offset);
  bool calc_ptr_range ();
  bool check_ptr_range ();
  bool check_recursive_call_arg ();
  void collect_recursive_call_args (auto_vec<tree> &array_args,
				    auto_vec<tree> &len_args,
				    auto_vec<basic_block> &blocks,
				    auto_vec<bool> &is_tail_recursive_call);
  void update_branch_range (basic_block bb);
  tree build_recursive_offset (tree len);
  tree build_recursive_ptr_range_max (basic_block bb, tree array,
				      tree offset);
  bool tail_recursive_call_p (gimple *stmt);
  bool return_bb_p (basic_block bb) const;
  bool calc_loop_var_range (loop_p loop);
  bool check_loop_exits (loop_p loop);
  bool fill_loop_var_range (loop_p loop, tree_code code, tree lhs,
			    tree rhs, tree step);
  bool fill_loop_ptr_range (loop_p loop, basic_block bb,
			    tree count_length);
  bool loop_header_p (basic_block bb);
  bool iterate_var_p (loop_p loop, tree var);
  tree get_loop_var (loop_p loop, tree iterate_var);
  bool find_var_range (tree var, basic_block bb);

public:
  cgraph_node *node = nullptr;
  tree array_param = nullptr;
  tree len_param = nullptr;
  int array_param_index = -1;
  int len_param_index = -1;
  tree array_main_var = nullptr;
  tree len_main_var = nullptr;
  tree signed_len_var = nullptr;
  tree elem_size = nullptr;
  unsigned elem_size_cst = 0;

  loop_p main_loop = nullptr;

  static constexpr unsigned PARAM_NUM = 2;

private:
  addr_analyzer analyzer;

  hash_set<tree> all_ptrs;
  hash_set<tree> visited_offset;
  hash_map<basic_block, basic_block> branch_start_map;
  hash_map<basic_block, value_range> len_range_map;
  std::map<tree, std::map<basic_block, value_range>> var_range;
  std::map<loop_p , hash_set<tree>> loop_ptrs;
  hash_set<basic_block> unreachable_blocks;

  static constexpr unsigned HOST_WIDE_INT len_param_min = 1;
  unsigned HOST_WIDE_INT len_param_max = 0;
};

class array_dse_edge
{
public:
  array_dse_edge (cgraph_edge *edge, array_dse_callee *callee);

  bool analyze ();
  bool fully_redundant ();
  tree get_bound_addr ();

private:
  bool find_local_array_from_arg ();
  bool check_array_usage ();
  bool collect_array_accesses ();
  bool check_array_access (gimple *stmt, tree var);
  bool check_array_address (gimple *stmt, tree addr);
  bool check_array_address (gphi *phi, tree addr);
  bool check_array_address (gassign *assign, tree addr);
  bool check_access_from_address (tree addr);
  bool check_access_kind (gimple *stmt, tree var);
  access_kind check_access_kind_iterate (tree var, auto_bitmap &visited);
  bool find_inner_array ();
  bool unique_use_p (tree source, gimple *unique_assign) const;
  bool initialize_assign_p (gimple *stmt) const;
  bool calc_read_bound ();
  value_range calc_ref_range (tree var);
  value_range calc_addr_range (tree var);
  value_range calc_offset_range (tree var);
  value_range calc_simple_loop_range (tree var);
  void collect_call_block_succs ();
  bool calc_array_arg_start ();
  bool check_optimized_area_rewrite ();
  bool check_full_write_elem (basic_block bb, tree var);
  bool array_index_of_addr_p (tree index, tree addr);
  tree strip_init_var (tree var, tree last_var);
  bool check_len_arg_range ();
  bool check_len_arg_lower_bound ();
  bool check_len_arg_upper_bound ();
  bool after_call_stmt_p (gimple *stmt);
  bool write_array_p (tree var);
  bool read_array_p (tree var);
  bool call_stmt_p (gimple *stmt) const;
  bool array_ref_p (tree var);
  bool array_addr_p (tree var);

public:
  cgraph_edge *call_edge = nullptr;
  array_dse_callee *callee = nullptr;

  tree array = nullptr;

private:
  unsigned array_size = 0;
  unsigned elem_size = 0;
  tree inner_array = nullptr;
  tree inner_elem_type = nullptr;

  hash_map<tree, gimple *> array_accesses;
  hash_map<tree, access_kind> access_kinds;
  hash_set<tree> array_address_vars;
  hash_set<basic_block> call_block_succs;

  HOST_WIDE_INT read_upper_bound = 0;
  HOST_WIDE_INT array_arg_start = 0;
  HOST_WIDE_INT len_arg_min = 0;
};

class ipa_array_dse
{
public:
  unsigned execute ();

private:
  bool find_array_dse_candidate_callees ();
  bool find_array_dse_candidate_edges ();
  bool apply_array_dse (array_dse_edge *edge);
  tree add_bound_param (tree param);
  tree find_array_main_var (array_dse_callee *callee);
  bool transform_new_callee (array_dse_callee *callee, cgraph_node *new_node);
  void rewrite_call_edge (cgraph_edge *edge, cgraph_node *new_node,
			  tree bound_ssa);

private:
  auto_vec<cgraph_node *> nodes;
  auto_delete_vec<array_dse_callee> candidate_callees;
  auto_delete_vec<array_dse_edge> candidate_edges;
};

}

#endif
