/* Propagate the alphabet of a constant byte array through pointer cursors.
   Copyright (C) 2026 Free Software Foundation, Inc.

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
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "fold-const.h"
#include "cgraph.h"
#include "tree-eh.h"
#include "tree-ssa.h"
#include "tree-dfa.h"
#include "value-range.h"
#include "hash-map.h"

namespace {

/* A supported search returns null or a pointer to a matching byte in INPUT.
   Keeping this semantic description separate from pointer-graph traversal
   allows new builtin adapters without changing the graph or range solver.
   An adapter must prove this provenance and match predicate for every defined
   call, including conversions of the search argument.  Do not infer them
   from an arbitrary function name or return type.  */
struct byte_search
{
  tree input;
  tree character;
  bool nul_terminated;
};

/* This is the only builtin-specific boundary.  Other search functions remain
   unsupported until their argument, result and failure semantics are modeled.
   NUL_TERMINATED records when whole-object failure folding requires a trailing
   zero.  The range solver enumerates a conservative superset of results and
   deliberately does not assume the first matching position is reachable.  */
static bool
describe_byte_search (gimple *stmt, byte_search &search)
{
  if (!gimple_call_builtin_p (stmt, BUILT_IN_STRCHR)
      || stmt_could_throw_p (cfun, stmt))
    return false;
  search.input = gimple_call_arg (stmt, 0);
  search.character = gimple_call_arg (stmt, 1);
  search.nul_terminated = true;
  return true;
}

static bool
constant_search_character (const byte_search &search, unsigned char &character)
{
  if (!tree_fits_shwi_p (search.character))
    return false;
  character = tree_to_shwi (search.character);
  return true;
}


/* Find a unique complete constant byte object at every leaf of the pointer
   definition graph.  Visiting a cycle once is sufficient: a cycle alone is
   not a proof, and at least one constant leaf must be found.  Every incoming
   PHI edge is checked, not just the loop entry edge.  No pointer loads,
   integer-to-pointer casts, or unknown/default definitions are used.
   Modeled search results are handled only where null-derived accesses are
   undefined; no other call results are used.

   Offsets need not be constant.  A defined byte access through an ordinary
   pointer derived by in-object pointer arithmetic still reads a byte of the
   same object.  Disable this reasoning for wrapping pointer arithmetic.
   Keep both individual graph walks and total work per function bounded.  */

static tree
constant_byte_object (tree pointer, unsigned &budget)
{
  auto_vec<tree, 32> todo;
  hash_set<tree> seen;
  tree common = NULL_TREE;
  unsigned local_budget = 256;
  todo.safe_push (pointer);
  while (!todo.is_empty ())
    {
      if (!local_budget-- || !budget)
	return NULL_TREE;
      --budget;
      tree value = todo.pop ();
      if (!POINTER_TYPE_P (TREE_TYPE (value))
	  || TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (value)))
	     != ADDR_SPACE_GENERIC)
	return NULL_TREE;
      if (seen.add (value))
	continue;
      if (TREE_CODE (value) == ADDR_EXPR)
	{
	  tree object = TREE_OPERAND (value, 0);
	  while (TREE_CODE (object) == ARRAY_REF)
	    object = TREE_OPERAND (object, 0);
	  if (TREE_CODE (object) == MEM_REF)
	    {
	      todo.safe_push (TREE_OPERAND (object, 0));
	      continue;
	    }
	  tree init = object;
	  if (VAR_P (object) && TREE_READONLY (object)
	      && !TREE_THIS_VOLATILE (object))
	    init = ctor_for_folding (object);
	  /* Require the complete array and its foldable STRING_CST initializer.
	     Partial initialization or inferred contents cannot justify enumerating
	     every byte of this very initializer in the cursor proof.  */
	  if (!init || TREE_CODE (init) != STRING_CST
	      || TREE_CODE (TREE_TYPE (object)) != ARRAY_TYPE
	      || !INTEGRAL_TYPE_P (TREE_TYPE (TREE_TYPE (object)))
	      || TYPE_PRECISION (TREE_TYPE (TREE_TYPE (object))) != 8
	      || !tree_fits_uhwi_p (TYPE_SIZE_UNIT (TREE_TYPE (object)))
	      || tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (object)))
		 != (unsigned) TREE_STRING_LENGTH (init)
	      || TREE_STRING_LENGTH (init) == 0
	      || TREE_STRING_LENGTH (init) > 256)
	    return NULL_TREE;
	  if (common && common != init)
	    return NULL_TREE;
	  common = init;
	  continue;
	}
      if (TREE_CODE (value) != SSA_NAME
	  || SSA_NAME_IS_DEFAULT_DEF (value)
	  || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (value))
	return NULL_TREE;
      gimple *def = SSA_NAME_DEF_STMT (value);
      if (gphi *phi = dyn_cast<gphi *> (def))
	{
	  if (gimple_phi_num_args (phi) > 64)
	    return NULL_TREE;
	  for (unsigned i = 0; i != gimple_phi_num_args (phi); ++i)
	    todo.safe_push (gimple_phi_arg_def (phi, i));
	}
      else if (is_gimple_assign (def))
	{
	  enum tree_code code = gimple_assign_rhs_code (def);
	  tree rhs = gimple_assign_rhs1 (def);
	  if (code != SSA_NAME && code != ADDR_EXPR
	      && code != POINTER_PLUS_EXPR && !CONVERT_EXPR_CODE_P (code))
	    return NULL_TREE;
	  if (!POINTER_TYPE_P (TREE_TYPE (rhs)))
	    return NULL_TREE;
	  todo.safe_push (rhs);
	}
      else
	{
	  byte_search search;
	  if (!flag_delete_null_pointer_checks
	      || !describe_byte_search (def, search))
	    return NULL_TREE;
	  /* A modeled result is null or points into INPUT.  A defined access
	     cannot dereference null here.  Unknown calls remain rejected.  */
	  todo.safe_push (search.input);
	}
    }
  return common;
}

/* Derive a byte set from fixed displacements relative to constant-character
   searches.  The complete pointer graph has already been checked by
   constant_byte_object.  A search result can only point at an occurrence of
   its character in that object.  Enumerating all occurrences is conservative
   even when the input cursor, or the first reachable terminator, is unknown.
   Preserve every PHI input.  Unsupported arithmetic and unclosed cursor cycles
   fall back to the whole-object alphabet; no loop bound is inferred.  */
static bool
constant_cursor_range (tree pointer, HOST_WIDE_INT displacement, tree object,
		       tree type, int_range_max &range, unsigned &budget)
{
  struct cursor { tree value; HOST_WIDE_INT displacement; };
  auto_vec<cursor, 32> todo;
  hash_map<tree, HOST_WIDE_INT> seen;
  cursor initial = {pointer, displacement};
  todo.safe_push (initial);
  unsigned local_budget = 256;
  bool searched = false;
  bool any = false;
  int size = TREE_STRING_LENGTH (object);
  const unsigned char *bytes
    = (const unsigned char *) TREE_STRING_POINTER (object);
  auto add_byte = [&] (HOST_WIDE_INT position)
    {
      /* Only a defined access is constrained.  Do not draw any conclusion
	 from an empty set of valid positions.  */
      if (position < 0 || position >= size)
	return;
      int value = bytes[position];
      if (!TYPE_UNSIGNED (type) && value >= 128)
	value -= 256;
      tree bound = build_int_cst (type, value);
      int_range<1> singleton (type, wi::to_wide (bound), wi::to_wide (bound));
      range.union_ (singleton);
      any = true;
    };
  while (!todo.is_empty ())
    {
      if (!local_budget-- || !budget)
	return false;
      --budget;
      cursor item = todo.pop ();
      tree value = item.value;
      HOST_WIDE_INT off = item.displacement;
      if (off < -512 || off > 512)
	return false;
      if (TREE_CODE (value) == ADDR_EXPR)
	{
	  poly_int64 unit_offset;
	  tree base = get_addr_base_and_unit_offset (TREE_OPERAND (value, 0),
						     &unit_offset);
	  HOST_WIDE_INT constant_offset;
	  if (!base || !unit_offset.is_constant (&constant_offset)
	      || constant_offset < 0 || constant_offset > size)
	    return false;
	  tree init = VAR_P (base) ? ctor_for_folding (base) : base;
	  if (init != object)
	    return false;
	  add_byte (constant_offset + off);
	  continue;
	}
      if (TREE_CODE (value) != SSA_NAME || SSA_NAME_IS_DEFAULT_DEF (value))
	return false;
      if (HOST_WIDE_INT *previous = seen.get (value))
	{
	  if (*previous != off)
	    return false;
	  continue;
	}
      seen.put (value, off);
      gimple *def = SSA_NAME_DEF_STMT (value);
      if (gphi *phi = dyn_cast<gphi *> (def))
	{
	  for (unsigned i = 0; i != gimple_phi_num_args (phi); ++i)
	    {
	      cursor next = {gimple_phi_arg_def (phi, i), off};
	      todo.safe_push (next);
	    }
	}
      else if (is_gimple_assign (def))
	{
	  tree rhs = gimple_assign_rhs1 (def);
	  enum tree_code code = gimple_assign_rhs_code (def);
	  if (code == POINTER_PLUS_EXPR)
	    {
	      tree delta = gimple_assign_rhs2 (def);
	      if (TREE_CODE (delta) != INTEGER_CST
		  || TYPE_PRECISION (TREE_TYPE (delta)) > HOST_BITS_PER_WIDE_INT)
		return false;
	      HOST_WIDE_INT d = wi::to_wide (delta).to_shwi ();
	      if (d < -512 || d > 512)
		return false;
	      off += d;
	    }
	  else if (code != SSA_NAME && code != ADDR_EXPR
		   && !CONVERT_EXPR_CODE_P (code))
	    return false;
	  cursor next = {rhs, off};
	  todo.safe_push (next);
	}
      else if (is_gimple_call (def))
	{
	  byte_search search;
	  unsigned char target;
	  if (!flag_delete_null_pointer_checks
	      || !describe_byte_search (def, search)
	      || !constant_search_character (search, target))
	    return false;
	  searched = true;
	  for (int i = 0; i != size; ++i)
	    {
	      if (!budget)
		return false;
	      --budget;
	      if (bytes[i] == target)
		add_byte (i + off);
	    }
	}
      else
	return false;
    }
  return searched && any;
}


static unsigned
constant_byte_range (function *fn)
{
  unsigned budget = 65536;
  unsigned cursor_budget = 65536;
  bool changed = false;
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	 !gsi_end_p (gsi) && budget; gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (gimple_has_volatile_ops (stmt))
	  continue;
	if (is_gimple_assign (stmt) && gimple_assign_single_p (stmt))
	  {
	    tree lhs = gimple_assign_lhs (stmt);
	    tree ref = gimple_assign_rhs1 (stmt);
	    tree type = TREE_TYPE (lhs);
	    if (TREE_CODE (lhs) != SSA_NAME || !INTEGRAL_TYPE_P (type)
		|| TYPE_PRECISION (type) != 8
		|| (TREE_CODE (ref) != MEM_REF
		    && TREE_CODE (ref) != ARRAY_REF)
		|| TREE_THIS_VOLATILE (ref))
	      continue;
	    tree ptr = TREE_CODE (ref) == MEM_REF
		       ? TREE_OPERAND (ref, 0) : build_fold_addr_expr (ref);
	    tree object = constant_byte_object (ptr, budget);
	    if (!object)
	      continue;
	    int_range_max range;
	    const unsigned char *bytes
	      = (const unsigned char *) TREE_STRING_POINTER (object);
	    bool refined = false;
	    if (flag_tree_constant_byte_range_cursor)
	      {
		HOST_WIDE_INT displacement = 0;
		bool known_displacement = true;
		if (TREE_CODE (ref) == MEM_REF)
		  {
		    tree offset = TREE_OPERAND (ref, 1);
		    known_displacement
		      = (TREE_CODE (offset) == INTEGER_CST
			 && TYPE_PRECISION (TREE_TYPE (offset))
			    <= HOST_BITS_PER_WIDE_INT);
		    if (known_displacement)
		      displacement = wi::to_wide (offset).to_shwi ();
		  }
		if (known_displacement)
		  refined = constant_cursor_range (ptr, displacement, object,
						   type, range, cursor_budget);
	      }
	    if (!refined && !flag_tree_constant_byte_range)
	      continue;
	    if (!refined)
	      for (int i = 0; i != TREE_STRING_LENGTH (object); ++i)
		{
		  int value = bytes[i];
		  if (!TYPE_UNSIGNED (type) && value >= 128)
		    value -= 256;
		  tree bound = build_int_cst (type, value);
		  int_range<1> singleton (type, wi::to_wide (bound),
					  wi::to_wide (bound));
		  range.union_ (singleton);
		}
	    if (set_range_info (lhs, range) && dump_file)
	      fprintf (dump_file,
		       refined ? "Constant byte cursor range: SSA %u, %d bytes.\n"
			       : "Constant byte object range: SSA %u, %d bytes.\n",
		       SSA_NAME_VERSION (lhs), TREE_STRING_LENGTH (object));
	  }
	else if (flag_tree_constant_byte_range && is_gimple_call (stmt))
	  {
	    byte_search search;
	    unsigned char value;
	    if (!describe_byte_search (stmt, search)
		|| !constant_search_character (search, value))
	      continue;
	    tree lhs = gimple_call_lhs (stmt);
	    if (!lhs)
	      continue;
	    tree object = constant_byte_object (search.input, budget);
	    if (!object)
	      continue;
	    int size = TREE_STRING_LENGTH (object);
	    const char *bytes = TREE_STRING_POINTER (object);
	    if ((search.nul_terminated && bytes[size - 1] != 0)
		|| memchr (bytes, value, size))
	      continue;
	    replace_call_with_value (&gsi, build_zero_cst (TREE_TYPE (lhs)));
	    changed = true;
	    if (dump_file)
	      fprintf (dump_file, "Constant byte object excludes search character.\n");
	  }
      }
  return changed ? TODO_update_ssa | TODO_cleanup_cfg : 0;
}

const pass_data pass_data_constant_byte_range =
{
  GIMPLE_PASS, "constbyterange", OPTGROUP_NONE, TV_TREE_CONSTANT_BYTE_RANGE,
  PROP_cfg | PROP_ssa, 0, 0, 0, 0
};

class pass_constant_byte_range : public gimple_opt_pass
{
public:
  pass_constant_byte_range (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_constant_byte_range, ctxt) {}
  opt_pass *clone () final override
  { return new pass_constant_byte_range (m_ctxt); }
  bool gate (function *) final override
  { return (flag_tree_constant_byte_range || flag_tree_constant_byte_range_cursor)
	   && !flag_wrapv_pointer
	   && BITS_PER_UNIT == 8; }
  unsigned execute (function *fn) final override
  { return constant_byte_range (fn); }
};

} // anonymous namespace

gimple_opt_pass *
make_pass_constant_byte_range (gcc::context *ctxt)
{
  return new pass_constant_byte_range (ctxt);
}
