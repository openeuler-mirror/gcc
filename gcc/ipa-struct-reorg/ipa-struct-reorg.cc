/* Struct-reorg optimizations.
   Copyright (C) 2016-2023 Free Software Foundation, Inc.
   Contributed by Andrew Pinski  <apinski@cavium.com>

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

/* This pass implements the structure reorganization organization
   (struct-reorg).

   Right now it handles just splitting off the hottest fields for a struct
   of 2 fields:
   struct s {
     type1 field1; // Hot field
     type2 field2;
   };
   s *v;
   into:
   struct s_hot {
     type1 field1;
   };
   struct c_cold {
     type2 field2;
   };
   s_hot *v_hot;
   s_cold *v_cold;

   TODO: This pass can be extended to more fields, and other alogrothims
   like reordering.

   This pass operate in four stages:
    1. All of the field accesses, declarations (struct types and pointers
       to that type) and struct types are scanned and recorded.  This includes
       global declarations.  Also record all allocation and freeing sites;
       this is needed for the rewriting phase.

       FIXME: If there is a top-level inline-asm, the pass immediately returns.

    2. Prune out the types which are considered escaping.
       Examples of types which are considered escaping:
       a. A declaration has been marked as having the attribute used or
	  has user defined alignment (type too).
       b. Accesses are via a BIT_FIELD_REF.
	  FIXME: Handle VECTOR_TYPE for this case.
       c. The "allocation" site is not a known builtin function.
       d. Casting to/from an integer.

    3. Analyze the types for which optimization to do.
       a. Split the fields into two different structs.
	  (FIXME: two field case handled only)
	  Look at all structs which contain two fields, if one of the fields
	  is hotter then split it and put it on the rewritting for accesses.
	  Allocations and freeing are marked to split into two functions;
	  all uses of that type will now be considered as two.
       b. Reorder fields hottest to the coldest.  TODO: Implement.

    4. Rewrite each access and allocation and free whichis marked as
       rewriting.

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "vec.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "cfg.h"
#include "cfghooks.h" /* For split_block.  */
#include "ssa.h"
#include "tree-dfa.h"
#include "fold-const.h"
#include "tree-inline.h"
#include "stor-layout.h"
#include "tree-into-ssa.h"
#include "tree-cfg.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "bitmap.h"
#include "ipa-struct-reorg.h"
#include "tree-eh.h"
#include "tree-ssa-live.h"  /* For remove_unused_locals.  */
#include "ipa-param-manipulation.h"
#include "gimplify-me.h"
#include "cfgloop.h"
#include "langhooks.h"
#include "cfgexpand.h"
#include "gimplify.h"
#include <map>

/* Check whether in C language or LTO with only C language.  */

bool
lang_c_p (void)
{
  const char *language_string = lang_hooks.name;

  if (!language_string)
    return false;

  if (lang_GNU_C ())
    return true;
  else if (strcmp (language_string, "GNU GIMPLE") == 0) // for LTO check
    {
      unsigned i = 0;
      tree t = NULL_TREE;

      FOR_EACH_VEC_SAFE_ELT (all_translation_units, i, t)
	{
	  language_string = TRANSLATION_UNIT_LANGUAGE (t);
	  if (language_string == NULL
	      || strncmp (language_string, "GNU C", 5)
	      || (language_string[5] != '\0'
		  && !(ISDIGIT (language_string[5]))))
	    return false;
	}
      return true;
    }
  return false;
}

namespace {

using namespace struct_reorg;
using namespace struct_relayout;

#define VOID_POINTER_P(type) \
  (POINTER_TYPE_P (type) && VOID_TYPE_P (TREE_TYPE (type)))

#define FC_DUMP_MSG(...) \
  do \
    { \
      if (dump_file && (dump_flags & TDF_DETAILS)) \
	{ \
	  fprintf (dump_file, "[field compress] "); \
	  fprintf (dump_file, __VA_ARGS__); \
	} \
    } while (0)

#define STRING_STARTS_WITH(s, suffix) \
  (strncmp (s, suffix, sizeof (suffix) - 1) == 0)

/* Flags for operand_equal_p to treat decls with the same name equal.  */

#define COMPARE_DECL_FLAGS (OEP_DECL_NAME | OEP_LEXICOGRAPHIC)

#define APPEND_GASSIGN_1(gsi, lhs, op, rhs) \
  gsi_insert_after (&gsi, gimple_build_assign (lhs, op, rhs), \
		    GSI_NEW_STMT)

#define APPEND_GASSIGN_2(gsi, lhs, op, rhs1, rhs2) \
  gsi_insert_after (&gsi, gimple_build_assign (lhs, op, rhs1, rhs2), \
		    GSI_NEW_STMT)

static void
set_var_attributes (tree var)
{
  if (!var)
    return;
  gcc_assert (TREE_CODE (var) == VAR_DECL);

  DECL_ARTIFICIAL (var) = 1;
  DECL_EXTERNAL (var) = 0;
  TREE_STATIC (var) = 1;
  TREE_PUBLIC (var) = 0;
  TREE_USED (var) = 1;
  DECL_CONTEXT (var) = NULL;
  TREE_THIS_VOLATILE (var) = 0;
  TREE_ADDRESSABLE (var) = 0;
  TREE_READONLY (var) = 0;
  if (is_global_var (var))
    set_decl_tls_model (var, TLS_MODEL_NONE);
}

/* Return true if TYPE is stdarg va_list type.  */

static inline bool
is_va_list_type (tree type)
{
  return TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (va_list_type_node);
}

static const char *
get_type_name (tree type)
{
  const char *tname = NULL;

  if (type == NULL)
    return NULL;

  if (TYPE_NAME (type) != NULL)
    {
      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	tname = IDENTIFIER_POINTER (TYPE_NAME (type));
      else if (DECL_NAME (TYPE_NAME (type)) != NULL)
	tname = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
    }
  return tname;
}

/* Return the inner most type for arrays and pointers of TYPE.  */

static tree
inner_type (tree type)
{
  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);
  return type;
}

/* Return true if TYPE is a type which struct reorg should handled.  */

static bool
handled_type (tree type)
{
  type = inner_type (type);
  if (TREE_CODE (type) == RECORD_TYPE)
    return !is_va_list_type (type);
  return false;
}

/* The gimplify_buildN API is moved to tree-vect-generic.c locally
   at commit b972e036f40c12b106f9070c3e8adea0eb8a45fa.

   The gimplify_buildN API is copied from gcc 10 implementation.
*/

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

/* Build a unary operation and gimplify it.  Emit code before GSI.
   Return the gimple_val holding the result.  */

static tree
gimplify_build1 (gimple_stmt_iterator *gsi, enum tree_code code, tree type,
		 tree a)
{
  tree ret;

  ret = fold_build1_loc (gimple_location (gsi_stmt (*gsi)), code, type, a);
  return force_gimple_operand_gsi (gsi, ret, true, NULL, true,
				   GSI_SAME_STMT);
}

/* Create a conditional expression as COND ? VAL1 : VAL2.  */

static inline tree
build_cond_expr (tree cond, tree val1, tree val2)
{
  if (TREE_CODE (TREE_TYPE (cond)) != BOOLEAN_TYPE)
    cond = fold_build2 (NE_EXPR, boolean_type_node, cond,
			build_zero_cst (TREE_TYPE (cond)));

  return fold_build3 (COND_EXPR, TREE_TYPE (val1), cond, val1, val2);
}

/* Given a struct/class pointer ADDR, and FIELD_DECL belonging to the
   struct/class, create a field reference expression.  */

static inline tree
build_field_ref (tree addr, tree field_decl)
{
  enum tree_code code;

  if (DECL_BIT_FIELD (field_decl))
    code = BIT_FIELD_REF;
  else
    code = COMPONENT_REF;

  tree base = TREE_CODE (addr) == MEM_REF ? addr : build_simple_mem_ref (addr);

  return build3 (code, TREE_TYPE (field_decl), base, field_decl, NULL_TREE);
}

/* Build a convert gimple to cast RHS to LHS.  */

tree
build_convert_gimple (tree lhs, tree rhs, gimple_stmt_iterator *gsi)
{
  tree ltype = TREE_TYPE (lhs);
  tree rtype = TREE_TYPE (rhs);
  if (types_compatible_p (ltype, rtype))
    return NULL_TREE;

  rhs = fold_build1 (CONVERT_EXPR, ltype, rhs);
  rhs = force_gimple_operand_gsi (gsi, rhs, true, NULL, true, GSI_SAME_STMT);
  return rhs;
}

/* Get the number of pointer layers.  */

int
get_ptr_layers (tree expr)
{
  int layers = 0;
  while (POINTER_TYPE_P (expr) || TREE_CODE (expr) == ARRAY_TYPE)
    {
      layers++;
      expr = TREE_TYPE (expr);
    }
  return layers;
}

/* Comparison pointer layers.  */

bool
cmp_ptr_layers (tree a, tree b)
{
  return get_ptr_layers (a) == get_ptr_layers (b);
}

/* Return true if the ssa_name comes from the void* parameter.  */

bool
is_from_void_ptr_parm (tree ssa_name)
{
  gcc_assert (TREE_CODE (ssa_name) == SSA_NAME);
  tree var = SSA_NAME_VAR (ssa_name);
  return (var && TREE_CODE (var) == PARM_DECL
	  && VOID_POINTER_P (TREE_TYPE (ssa_name)));
}

/* Check if STMT is a gimple assign whose rhs code is CODE.  */

static bool
gimple_assign_rhs_code_p (gimple *stmt, enum tree_code code)
{
  return stmt && is_gimple_assign (stmt)
	 && gimple_assign_rhs_code (stmt) == code;
}

static fc_field *
find_fc_field (const auto_vec<fc_field *> &fc_fields, tree field)
{
  for (auto *fc_f : fc_fields)
    if (fc_f->field == field)
      return fc_f;

  return NULL;
}

/* Return true if the stmt is a copy/convert to integer.  */

static bool
is_copy_int (const gimple *stmt)
{
  if (!is_gimple_assign (stmt))
    return NULL_TREE;

  tree rhs = gimple_assign_rhs1 (stmt);

  if (gimple_assign_single_p (stmt))
    if (TREE_CODE (rhs) == SSA_NAME)
      return true;

  if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt)))
    {
      tree lhs = gimple_assign_lhs (stmt);

      if (TREE_CODE (rhs) == SSA_NAME
	  && TREE_CODE (TREE_TYPE (lhs)) == INTEGER_TYPE)
	return true;
    }

  return false;
}

/* Strip the copy with typecasting of int or unsigned int.  */

static gimple *
strip_copy_stmts (gimple *stmt)
{
  while (is_copy_int (stmt))
    stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));

  return stmt;
}

const char *
get_func_name (tree decl)
{
  if (!decl || TREE_CODE (decl) != FUNCTION_DECL || !DECL_NAME (decl))
    return NULL;

  tree decl_name = DECL_NAME (decl);
  if (TREE_CODE (decl_name) != IDENTIFIER_NODE)
    return NULL;

  return IDENTIFIER_POINTER (decl_name);
}

/* Compare the gimple order of input_ssa for fc fields.  */

static int
input_order_cmp (const void *p, const void *q)
{
  const fc_field *a = *static_cast<const fc_field *const *> (p);
  const fc_field *b = *static_cast<const fc_field *const *> (q);

  gimple *ga = SSA_NAME_DEF_STMT (a->input_ssa);
  gimple *gb = SSA_NAME_DEF_STMT (b->input_ssa);

  if (gimple_uid (ga) < gimple_uid (gb))
    return -1;
  else if (gimple_uid (ga) > gimple_uid (gb))
    return 1;
  else
    return 0;
}

/* Called by walk_tree to check if ssa_name DATA exists in an expression.  */

static tree
check_for_ssa (tree *opnd_ptr, int *walk_subtrees ATTRIBUTE_UNUSED, void *data)
{
  tree ssa = (tree) data;
  if (*opnd_ptr == ssa)
    return ssa;

  return NULL_TREE;
}

/* Helper to create a function declaration together with arguments and result
   declarations.  */

static tree
create_new_fn_decl (char *fn_name, int n_args, tree *arg_types,
		    tree return_type)
{
  tree fn_type = build_function_type_array (return_type, n_args, arg_types);
  tree fndecl = build_fn_decl (fn_name, fn_type);
  tree id = get_identifier (fn_name);
  SET_DECL_ASSEMBLER_NAME (fndecl, id);
  DECL_NAME (fndecl) = id;
  DECL_ARTIFICIAL (fndecl) = 1;
  DECL_EXTERNAL (fndecl) = 0;
  DECL_CONTEXT (fndecl) = NULL_TREE;
  DECL_INITIAL (fndecl) = make_node (BLOCK);
  DECL_STATIC_CONSTRUCTOR (fndecl) = 0;

  /* Function result declairation.  */
  tree resdecl
    = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, return_type);
  DECL_RESULT (fndecl) = resdecl;

  /* Function arguments.  */
  tree prev_arg = NULL_TREE;
  for (int i = 0; i < n_args; i++)
    {
      tree arg_decl
	= build_decl (UNKNOWN_LOCATION, PARM_DECL, NULL, arg_types[i]);
      DECL_ARTIFICIAL (arg_decl) = 1;
      DECL_IGNORED_P (arg_decl) = 1;
      TREE_USED (arg_decl) = 1;
      DECL_CONTEXT (arg_decl) = fndecl;
      DECL_ARG_TYPE (arg_decl) = arg_types[i];
      TREE_READONLY (arg_decl) = 1;
      if (prev_arg)
	TREE_CHAIN (prev_arg) = arg_decl;
      else
	DECL_ARGUMENTS (fndecl) = arg_decl;
      prev_arg = arg_decl;
    }

  return fndecl;
}

static void
release_srdecl_ssa_name (srdecl *srd)
{
  if (!srd->has_new_decl ())
    return;

  tree ssa_name = NULL_TREE;
  if (srd->argumentnum >= 0)
    ssa_name = ssa_default_def (cfun, srd->decl);
  else if (TREE_CODE (srd->decl) == SSA_NAME)
    ssa_name = srd->decl;

  if (ssa_name && num_imm_uses (ssa_name) == 0)
    release_ssa_name (ssa_name);
}

static char *
append_suffix (const char *s1, unsigned suffix)
{
  char s2[32];
  sprintf (s2, "%u", suffix);
  return concat (s1, s2, NULL);
}

static unsigned HOST_WIDE_INT
get_bitsize (tree field)
{
  tree bitsize = DECL_BIT_FIELD (field) ? DECL_SIZE (field)
					: TYPE_SIZE (TREE_TYPE (field));
  return tree_to_uhwi (bitsize);
}

/* Generate SSA_NAME for the given var.  */

static tree
generate_ssa_name (tree var, gimple_stmt_iterator *gsi)
{
  if (TREE_CODE (var) == SSA_NAME)
    return var;

  tree name = make_ssa_name (TREE_TYPE (var));
  gimple *stmt = gimple_build_assign (name, var);
  gsi_insert_after (gsi, stmt, GSI_NEW_STMT);

  return name;
}

/* Get type node by bit precision and sign.  */

static tree
get_integer_type_node (unsigned precision, bool is_unsigned)
{
  switch (precision)
    {
      case 64:
	return is_unsigned ? long_long_unsigned_type_node
			   : long_long_integer_type_node;
      case 32:
	return is_unsigned ? unsigned_type_node : integer_type_node;
      case 16:
	return is_unsigned ? short_unsigned_type_node
			   : short_integer_type_node;
      case 8:
	return is_unsigned ? unsigned_char_type_node
			   : signed_char_type_node;
      default:
	return NULL_TREE;
    }
}

/* Enum the struct layout optimize level,
   which should be the same as the option -fstruct-reorg=.  */

enum struct_layout_opt_level
{
  NONE = 0,
  STRUCT_SPLIT = 1 << 0,
  COMPLETE_STRUCT_RELAYOUT = 1 << 1,
  STRUCT_REORDER_FIELDS = 1 << 2,
  DEAD_FIELD_ELIMINATION = 1 << 3,
  POINTER_COMPRESSION_SAFE = 1 << 4,
  POINTER_COMPRESSION_UNSAFE = 1 << 5,
  SEMI_RELAYOUT = 1 << 6
};

enum class fc_level
{
  NONE,
  STATIC,
  DYNAMIC
};

fc_level current_fc_level;

srfunction *current_function;
vec<srfunction *> csrfun_stack;

class csrfun_context
{
public:
  csrfun_context (srfunction *srfun)
  {
    csrfun_stack.safe_push (current_function);
    current_function = srfun;

    push_cfun (DECL_STRUCT_FUNCTION (srfun->node->decl));
  }

  ~csrfun_context ()
  {
    pop_cfun ();
    current_function = csrfun_stack.pop ();
  }
};

#define SET_CFUN(srfn) csrfun_context csrfn_ctx(srfn);

/* RAII class to change current dump_file and dump_flags,
   and restore when the object goes out of scope.  */

class dump_file_saver
{
public:
  dump_file_saver (FILE *file, dump_flags_t flags)
  {
    old_dump_file = dump_file;
    old_dump_flags = dump_flags;
    dump_file = file;
    dump_flags = flags;
  }
  ~dump_file_saver ()
  {
    dump_file = old_dump_file;
    dump_flags = old_dump_flags;
  }
private:
  FILE *old_dump_file;
  dump_flags_t old_dump_flags;
};

#define SET_DUMP_FILE(file, flags) dump_file_saver fd_saver(file, flags);

/* Defines the target pointer size of compressed pointer, which should be 8,
   16, 32.  */

static int compressed_size = 32;

static bool is_result_of_mult (tree arg, tree *num, tree struct_size);
static bool isptrptr (tree type);
void get_base (tree &base, tree expr);

static unsigned int current_layout_opt_level;
hash_map<tree, tree> replace_type_map;
hash_map<tree, tree> semi_relayout_map;

/* Return true if one of these types is created by struct-reorg.  */

static bool
is_replace_type (tree type1, tree type2)
{
  if (replace_type_map.is_empty ())
    return false;
  if (type1 == NULL_TREE || type2 == NULL_TREE)
    return false;
  tree *type_value = replace_type_map.get (type1);
  if (type_value)
    if (types_compatible_p (*type_value, type2))
      return true;
  type_value = replace_type_map.get (type2);
  if (type_value)
    if (types_compatible_p (*type_value, type1))
      return true;
  return false;
}

} // anon namespace

namespace struct_reorg {

hash_map <tree, auto_vec <tree>> fields_to_finish;

/* Constructor of srfunction.  */

srfunction::srfunction (cgraph_node *n)
  : node (n),
    old (NULL),
    newnode (NULL),
    newf (NULL),
    is_safe_func (false)
{
}

/* Add an ARG to the list of arguments for the function.  */

void
srfunction::add_arg (srdecl *arg)
{
  args.safe_push (arg);
}

/* Dump the SRFUNCTION to the file FILE.  */

void
srfunction::dump (FILE *file)
{
  if (node)
    {
      fprintf (file, "function : ");
      print_generic_expr (file, node->decl);
      fprintf (file, " with arguments: ");
      for (unsigned i = 0; i < args.length (); i++)
	{
	  if (i == 0)
	    fprintf (file, "\n  ");
	  else
	    fprintf (file, "\n,  ");
	  args[i]->dump (file);
	}

      fprintf (file, "\nuses globals: ");
      for (unsigned i = 0; i < globals.length (); i++)
	{
	  fprintf (file, "\n  ");
	  globals[i]->dump (file);
	}

      fprintf (file, "\ndecls: ");
    }
  else
    fprintf (file, "globals : ");

  for (unsigned i = 0; i < decls.length (); i++)
    {
      fprintf (file, "\n  ");
      decls[i]->dump (file);
    }
}

/* Simple dump the SRFUNCTION to the file FILE;
   used so it is not recusive.  */

void
srfunction::simple_dump (FILE *file)
{
  print_generic_expr (file, node->decl);
}

/* Constructor of FIELD.  */

srfield::srfield (tree field, srtype *base)
  : offset (int_byte_position (field)),
    fieldtype (TREE_TYPE (field)),
    fielddecl (field),
    base (base),
    type (NULL),
    clusternum (0),
    field_access (EMPTY_FIELD),
    fc_f (NULL),
    field_class (NULL)
{
  for (int i = 0; i < max_split; i++)
    newfield[i] = NULL_TREE;
}

/* Constructor of TYPE.  */

srtype::srtype (tree type)
  : type (type),
    chain_type (false),
    escapes (does_not_escape),
    pc_gptr (NULL_TREE),
    visited (false),
    pc_candidate (false),
    has_legal_alloc_num (false),
    has_alloc_array (0),
    semi_relayout (false),
    bucket_parts (0),
    fc_info (NULL)
{
  for (int i = 0; i < max_split; i++)
    newtype[i] = NULL_TREE;

  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL)
	{
	  if (current_fc_level != fc_level::DYNAMIC
	      && DECL_BIT_FIELD (field))
	    {
	      escapes = escape_bitfields;
	      continue;
	    }
	  else if (!DECL_SIZE (field)
		   || TREE_CODE (DECL_SIZE (field)) != INTEGER_CST)
	    {
	      escapes = escape_variable_sized_array;
	      break;
	    }
	  srfield *t = new srfield (field, this);
	  fields.safe_push (t);
	}
    }
}

/* Check it if all fields in the RECORD_TYPE are referenced.  */

bool
srtype::has_dead_field (void)
{
  bool may_dfe = false;
  srfield *this_field;
  unsigned i;
  FOR_EACH_VEC_ELT (fields, i, this_field)
    {
      /* Function pointer members are not processed, because DFE
	 does not currently support accurate analysis of function
	 pointers, and we have not identified specific use cases.  */
      if (!(this_field->field_access & READ_FIELD)
	 && !FUNCTION_POINTER_TYPE_P (this_field->fieldtype))
	{
	  /* Fields with escape risks should not be processed. */
	  if (this_field->type == NULL
	      || (this_field->type->escapes == does_not_escape))
	    {
	      may_dfe = true;
	      break;
	    }
	}
    }
  return may_dfe;
}

/* Mark the type as escaping type E at statement STMT.  */

void
srtype::mark_escape (escape_type e, gimple *stmt)
{
  /* Once the type has escaped, it should never
     change back to non escaping.  */
  gcc_assert (e != does_not_escape);
  if (has_escaped ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nO type: ");
	  simple_dump (dump_file);
	  fprintf (dump_file, " has already escaped.");
	  fprintf (dump_file, " old = \"%s\" ",
		   escape_type_string[escapes - 1]);
	  fprintf (dump_file, " new = \"%s\"\n", escape_type_string[e - 1]);
	  if (stmt)
	    print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      return;
    }
  escapes = e;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nN type: ");
      simple_dump (dump_file);
      fprintf (dump_file, " new = \"%s\"\n", escape_reason ());
      if (stmt)
	print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "\n");
    }
}

/* Create a global header for compressed struct.  */

void
srtype::create_global_ptr_for_pc ()
{
  if (!pc_candidate || pc_gptr != NULL_TREE)
    return;

  const char *type_name = get_type_name (type);
  gcc_assert (type_name != NULL);

  char *gptr_name = concat (type_name, "_pc", NULL);
  tree new_name = get_identifier (gptr_name);
  tree new_type = build_pointer_type (newtype[0]);
  tree new_var = build_decl (UNKNOWN_LOCATION, VAR_DECL, new_name, new_type);
  set_var_attributes (new_var);
  pc_gptr = new_var;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nType: %s has create global header for pointer"
	       " compression: %s\n", type_name, gptr_name);

  free (gptr_name);
}

/* Add FIELD to the list of fields that use this type.  */

void
srtype::add_field_site (srfield *field)
{
  field_sites.safe_push (field);
}

/* Constructor of DECL.  */

srdecl::srdecl (srtype *tp, tree decl, int argnum, tree orig_type)
  : type (tp),
    decl (decl),
    func (NULL_TREE),
    argumentnum (argnum),
    visited (false),
    orig_type (orig_type)
{
  if (TREE_CODE (decl) == SSA_NAME)
    func = current_function_decl;
  else if (!is_global_var (decl))
    func = DECL_CONTEXT (decl);
  for (int i = 0; i < max_split; i++)
    newdecl[i] = NULL_TREE;
}

/* Find DECL in the function.  */

srdecl *
srfunction::find_decl (tree decl)
{
  for (unsigned i = 0; i < decls.length (); i++)
    if (decls[i]->decl == decl)
      return decls[i];
  return NULL;
}

/* Record DECL of the TYPE with argument num ARG.  */

srdecl *
srfunction::record_decl (srtype *type, tree decl, int arg, tree orig_type)
{
  // Search for the decl to see if it is already there.
  srdecl *decl1 = find_decl (decl);

  if (decl1)
    {
      /* Added the orig_type information.  */
      if (!decl1->orig_type && orig_type && isptrptr (orig_type))
	decl1->orig_type = orig_type;
      return decl1;
    }

  gcc_assert (type);

  orig_type = isptrptr (TREE_TYPE (decl)) ? TREE_TYPE (decl) : orig_type;
  decl1 = new srdecl (type, decl, arg, isptrptr (orig_type) ? orig_type : NULL);
  decls.safe_push (decl1);
  return decl1;
}

/* A function is either partially cloned or fully cloned (versioning).  */

bool
srfunction::partial_clone_p ()
{
  return fc_path.start_stmt != NULL;
}

bool
srfunction::entry_function_p ()
{
  return strcmp (node->name (), "main") == 0 && !node->callers;
}

/* Find the field at OFF offset.  */

srfield *
srtype::find_field (unsigned HOST_WIDE_INT off)
{
  unsigned int i;
  srfield *field;

  /* FIXME: handle array/struct field inside the current struct.  */
  /* NOTE This does not need to be fixed to handle libquatumn.  */
  FOR_EACH_VEC_ELT (fields, i, field)
    {
      if (off == field->offset)
	return field;
    }
  return NULL;
}

/* Find the field according to field decl.  */
srfield *
srtype::find_field_by_decl (tree fielddecl)
{
  for (auto *field : fields)
    if (operand_equal_p (fielddecl, field->fielddecl, COMPARE_DECL_FLAGS))
      return field;

  return NULL;
}

/* Add the function FN to the list of functions if it
   is there not already.  */

void
srtype::add_function (srfunction *fn)
{
  unsigned decluid;
  unsigned i;
  decluid = DECL_UID (fn->node->decl);

  srfunction *fn1;
  // Search for the decl to see if it is already there.
  FOR_EACH_VEC_ELT (functions, i, fn1)
    {
      if (DECL_UID (fn1->node->decl) == decluid)
	return;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Recording new function: %u.\n", decluid);

  functions.safe_push (fn);
}

/* Dump out the type structure to FILE.  */

void
srtype::dump (FILE *f)
{
  unsigned int i;
  srfield *field;
  srfunction *fn;
  sraccess *access;

  if (chain_type)
    fprintf (f, "chain decl ");

  fprintf (f, "type : ");
  print_generic_expr (f, type);
  fprintf (f, "(%d) { ", TYPE_UID (type));
  if (escapes != does_not_escape)
    fprintf (f, "escapes = \"%s\"", escape_reason ());
  fprintf (f, "\nfields = {\n");
  FOR_EACH_VEC_ELT (fields, i, field)
    field->dump (f);
  fprintf (f, "}\n ");

  fprintf (f, "\naccesses = {\n");
  FOR_EACH_VEC_ELT (accesses, i, access)
    access->dump (f);
  fprintf (f, "}\n ");

  fprintf (f, "\nfunctions = {\n");
  FOR_EACH_VEC_ELT (functions, i, fn)
    fn->simple_dump (f);
  fprintf (f, "}\n");
  fprintf (f, "}\n");
}

/* A simplified dump out the type structure to FILE.  */

void
srtype::simple_dump (FILE *f)
{
  print_generic_expr (f, type);
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
    fprintf (f, "(%d)", TYPE_UID (type));
}

/* Analyze the type and decide what to be done with it.  */

void
srtype::analyze (void)
{
  /* Chain decl types can't be split
     so don't try.  */
  if (chain_type)
    return;

  /* If there is only one field then there is nothing
     to be done.  */
  if (fields.length () == 1)
    return;

  /*  For now we unconditionally split only structures with 2 fields
      into 2 different structures.  In future we intend to add profile
      info and/or static heuristics to differentiate splitting process.  */
  if (fields.length () == 2)
    {
      /* Currently, when the replacement structure type exists,
	 we only split the replacement structure. */
      for (hash_map<tree, tree>::iterator it = replace_type_map.begin ();
	   it != replace_type_map.end (); ++it)
	{
	  if (types_compatible_p ((*it).second, this->type))
	    return;
	}
      fields[1]->clusternum = 1;
    }

  /* Otherwise we do nothing.  */
  if (fields.length () >= 3)
    return;
}

/* Create the new fields for this field.  */

void
srfield::create_new_fields (tree newtype[max_split],
			    tree newfields[max_split],
			    tree newlast[max_split])
{
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
    {
      create_new_reorder_fields (newtype, newfields, newlast);
      return;
    }

  tree nt[max_split];

  for (unsigned i = 0; i < max_split; i++)
    nt[i] = NULL;

  if (type == NULL)
    nt[0] = fieldtype;
  else
    memcpy (nt, type->newtype, sizeof (type->newtype));

  for (unsigned i = 0; i < max_split && nt[i] != NULL; i++)
    {
      tree field = make_node (FIELD_DECL);
      if (nt[1] != NULL && DECL_NAME (fielddecl))
	{
	  const char *tname = IDENTIFIER_POINTER (DECL_NAME (fielddecl));
	  char id[10];
	  char *name;

	  sprintf (id, "%d", i);
	  name = concat (tname, ".reorg.", id, NULL);
	  DECL_NAME (field) = get_identifier (name);
	  free (name);
	}
      else
	DECL_NAME (field) = DECL_NAME (fielddecl);

      TREE_TYPE (field) = reconstruct_complex_type (
			    TREE_TYPE (fielddecl), nt[i]);
      DECL_SOURCE_LOCATION (field) = DECL_SOURCE_LOCATION (fielddecl);
      SET_DECL_ALIGN (field, DECL_ALIGN (fielddecl));
      DECL_USER_ALIGN (field) = DECL_USER_ALIGN (fielddecl);
      TREE_ADDRESSABLE (field) = TREE_ADDRESSABLE (fielddecl);
      DECL_NONADDRESSABLE_P (field) = !TREE_ADDRESSABLE (fielddecl);
      TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (fielddecl);
      DECL_CONTEXT (field) = newtype[clusternum];

      if (newfields[clusternum] == NULL)
	newfields[clusternum] = newlast[clusternum] = field;
      else
	{
	  DECL_CHAIN (newlast[clusternum]) = field;
	  newlast[clusternum] = field;
	}
      newfield[i] = field;
    }
}

/* Reorder fields.  */

void
srfield::reorder_fields (tree newfields[max_split], tree newlast[max_split],
			 tree &field)
{
  /* Reorder fields in descending.
     newfields: always stores the first member of the chain
		and with the largest size.
     field: indicates the node to be inserted.  */
  if (newfields[clusternum] == NULL)
    {
      newfields[clusternum] = field;
      newlast[clusternum] = field;
    }
  else
    {
      tree tmp = newfields[clusternum];
      auto field_bitsize = get_bitsize (field);
      auto tmp_bitsize = get_bitsize (tmp);
      if (field_bitsize > tmp_bitsize)
	{
	  DECL_CHAIN (field) = tmp;
	  newfields[clusternum] = field;
	}
      else
	{
	  while (DECL_CHAIN (tmp)
		 && field_bitsize <= get_bitsize (DECL_CHAIN (tmp)))
	    tmp = DECL_CHAIN (tmp);

	  /* Now tmp size > field size
	     insert field: tmp -> xx ==> tmp -> field -> xx.  */
	  DECL_CHAIN (field) = DECL_CHAIN (tmp); // field -> xx
	  DECL_CHAIN (tmp) = field; // tmp -> field
	}
    }
}

/* Create the new reorder fields for this field.
   newtype[max_split]: srtype's member variable,
   newfields[max_split]: created by create_new_type func,
   newlast[max_split]: created by create_new_type func.  */

void
srfield::create_new_reorder_fields (tree newtype[max_split],
				    tree newfields[max_split],
				    tree newlast[max_split])
{
  /* For dynamic shadow.  */
  if (current_fc_level == fc_level::DYNAMIC && fc_f && fc_f->original)
    {
      newfield[0] = NULL_TREE;
      return;
    }

  /* newtype, corresponding to newtype[max_split] in srtype.  */
  tree nt = NULL_TREE;
  if (type == NULL)
    /* Common var.  */
    nt = fc_f ? fc_f->new_type : fieldtype;
  else
    /* RECORD_TYPE var.  */
    nt = type->has_escaped () ? type->type : type->newtype[0];

  tree field = make_node (FIELD_DECL);

  /* Used for recursive types.
     fields_to_finish: hase_map in the format of "type: {fieldA, fieldB}",
     key : indicates the original type,
     vaule: filed that need to be updated to newtype.  */
  if (nt == NULL)
    {
      nt = make_node (RECORD_TYPE);
      auto_vec <tree> &fields
	= fields_to_finish.get_or_insert (inner_type (type->type));
      fields.safe_push (field);
    }

  if (type == NULL)
    {
      DECL_NAME (field) = DECL_NAME (fielddecl);
      /* Common members do not need to reconstruct.
       Otherwise, int* -> int** or void* -> void**.  */
      TREE_TYPE (field) = nt;
      SET_DECL_ALIGN (field, DECL_ALIGN (fielddecl));
    }
  else if (type->pc_candidate)
    {
      const char *old_name = IDENTIFIER_POINTER (DECL_NAME (fielddecl));
      char *new_name = concat (old_name, "_pc", NULL);
      DECL_NAME (field) = get_identifier (new_name);
      free (new_name);
      TREE_TYPE (field) = make_unsigned_type (compressed_size);
      SET_DECL_ALIGN (field, compressed_size);
    }
  else
    {
      TREE_TYPE (field) = reconstruct_complex_type (TREE_TYPE (fielddecl), nt);
      DECL_NAME (field) = DECL_NAME (fielddecl);
      SET_DECL_ALIGN (field, DECL_ALIGN (fielddecl));
    }

  DECL_SOURCE_LOCATION (field) = DECL_SOURCE_LOCATION (fielddecl);
  DECL_USER_ALIGN (field) = DECL_USER_ALIGN (fielddecl);
  TREE_ADDRESSABLE (field) = TREE_ADDRESSABLE (fielddecl);
  DECL_NONADDRESSABLE_P (field) = !TREE_ADDRESSABLE (fielddecl);
  TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (fielddecl);
  DECL_CONTEXT (field) = newtype[clusternum];

  fc_type_info *info = base->fc_info;
  if (info && (info->static_fc_p || info->dynamic_fc_p))
    {
      DECL_PACKED (field) = 1;
      DECL_BIT_FIELD (field) = DECL_BIT_FIELD (fielddecl);
      if (DECL_BIT_FIELD (field))
	DECL_SIZE (field) = DECL_SIZE (fielddecl);

      if (fc_f)
	{
	  /* Always not align compressed fields.  */
	  SET_DECL_ALIGN (field, 0);

	  if (fc_f->bits)
	    {
	      DECL_BIT_FIELD (field) = 1;
	      DECL_SIZE (field) = bitsize_int (fc_f->bits);
	      DECL_NONADDRESSABLE_P (field) = 1;
	      /* Build unsigned bitfield integer type.  */
	      nt = build_nonstandard_integer_type (fc_f->bits, 1);
	      TREE_TYPE (field) = nt;
	      fc_f->new_type = nt;
	    }
	}
    }

  reorder_fields (newfields, newlast, field);

  /* srfield member variable, which stores the new field decl.  */
  newfield[0] = field;
}

bool
srfield::dead_field_p ()
{
  return current_layout_opt_level & DEAD_FIELD_ELIMINATION
	       && !(field_access & READ_FIELD)
	       && !FUNCTION_POINTER_TYPE_P (fieldtype);
}

bool
srfield::dfc_type_change_p ()
{
  return fc_f && fc_f->cond
	 && fc_f->cond->old_type != TREE_TYPE (newfield[0]);
}

fc_closure *
srfield::get_closure ()
{
  return &(field_class->closure);
}

/* Given a struct s whose fields has already reordered by size, we try to
   combine fields less than 8 bytes together to 8 bytes.  Example:
   struct s {
     uint64_t a,
     uint32_t b,
     uint32_t c,
     uint32_t d,
     uint16_t e,
     uint8_t f
   }

   We allocate memory for arrays of struct S, before semi-relayout, their
   layout in memory is shown as below:
   [a,b,c,d,e,f,padding;a,b,c,d,e,f,padding;...]

   During semi-relayout, we put a number of structs into a same region called
   bucket.  The number is determined by param realyout-bucket-capacity-level.
   Using 1024 here as example.  After semi-relayout, the layout in a bucket is
   shown as below:
   part1 [a;a;a...]
   part2 [b,c;b,c;b,c;...]
   part3 [d,e,f,pad;d,e,f,pad;d,e,f,pad;...]

   In the last bucket, if the amount of rest structs is less than the capacity
   of a bucket, the rest of allcated memory will be wasted as padding.  */

unsigned
srtype::calculate_bucket_size ()
{
  unsigned parts = 0;
  unsigned bit_sum = 0;
  unsigned relayout_offset = 0;
  /* Currently, limit each 8 bytes with less than 2 fields.  */
  unsigned curr_part_num = 0;
  unsigned field_num = 0;
  for (tree f = TYPE_FIELDS (newtype[0]); f; f = DECL_CHAIN (f))
    {
      unsigned size = TYPE_PRECISION (TREE_TYPE (f));
      bit_sum += size;
      field_num++;
      if (++curr_part_num > 2 || bit_sum > 64)
	{
	  bit_sum = size;
	  parts++;
	  relayout_offset = relayout_part_size * parts;
	  curr_part_num = 1;
	}
      else
	{
	  relayout_offset = relayout_part_size * parts + (bit_sum - size) / 8;
	}
      new_field_offsets.put (f, relayout_offset);
    }
  /* Donnot relayout a struct with only one field after DFE.  */
  if (field_num == 1)
    return 0;
  bucket_parts = ++parts;
  return parts * relayout_part_size;
}

bool
srtype::has_recursive_field_type ()
{
  /* A dead field is ignored as it will be removed in transformation.  */
  for (const auto &srf : fields)
    if (srf->type == this && !srf->dead_field_p ())
      return true;
  return false;
}

void
srtype::check_fc_fields ()
{
  if (!fc_info || (!fc_info->static_fc_p && !fc_info->dynamic_fc_p))
    return;

  for (auto *srf : fields)
    {
      tree field = srf->fielddecl;
      if (fc_info->static_fc_p)
	srf->fc_f = find_fc_field (fc_info->static_fc_fields, field);
      else
	{
	  srf->fc_f = find_fc_field (fc_info->dynamic_shadow_fields, field);
	  if (!srf->fc_f)
	    srf->fc_f = find_fc_field (fc_info->dynamic_fc_fields, field);
	}
    }
}

bool
srtype::reorg_name_p ()
{
  const char *name = get_type_name (type);
  return name && strstr (name, ".reorg");
}

bool
srtype::has_escaped ()
{
  return escapes != does_not_escape
	 && (current_fc_level != fc_level::DYNAMIC
	     || !reorg_name_p ());
}

/* Create the new TYPE corresponding to THIS type.  */

bool
srtype::create_new_type (void)
{
  /* If the type has been visited,
     then return if a new type was
     created or not.  */
  if (visited)
    return has_new_type ();

  visited = true;

  if (has_escaped ())
    {
      newtype[0] = type;
      return false;
    }

  bool createnewtype = false;
  unsigned maxclusters = 0;

  /* Create a new type for each field.  */
  for (unsigned i = 0; i < fields.length (); i++)
    {
      srfield *field = fields[i];
      if (field->type)
	createnewtype |= field->type->create_new_type ();
      if (field->clusternum > maxclusters)
	maxclusters = field->clusternum;
    }

  /* If the fields' types did have a change or
     we are not splitting the struct into two clusters,
     then just return false and don't change the type.  */
  if (!createnewtype && maxclusters == 0
      && current_layout_opt_level < STRUCT_REORDER_FIELDS)
    {
      newtype[0] = type;
      return false;
    }

  /* Should have at most max_split clusters.  */
  gcc_assert (maxclusters < max_split);

  /* Record the first member of the field chain.  */
  tree newfields[max_split];
  tree newlast[max_split];

  maxclusters++;

  const char *tname = get_type_name (type);

  for (unsigned i = 0; i < maxclusters; i++)
    {
      newfields[i] = NULL_TREE;
      newlast[i] = NULL_TREE;
      newtype[i] = make_node (RECORD_TYPE);

      char *name = NULL;
      char id[10];
      sprintf (id, "%d", i);
      if (tname)
	{
	  name = concat (tname, ".reorg.", id, NULL);
	  tree name_id = get_identifier (name);
	  TYPE_STUB_DECL (newtype[i])
	    = build_decl (UNKNOWN_LOCATION, TYPE_DECL, name_id, newtype[i]);
	  TYPE_NAME (newtype[i]) = name_id;
	  free (name);
	}
    }

  check_fc_fields ();
  for (unsigned i = 0; i < fields.length (); i++)
    {
      srfield *f = fields[i];
      if (f->dead_field_p ())
	{
	  /* Fields with escape risks should not be processed. */
	  if (f->type == NULL || (f->type->escapes == does_not_escape))
	    continue;
	}
      f->create_new_fields (newtype, newfields, newlast);
    }

  /* No reason to warn about these structs since the warning would
     have happened already.  */
  int save_warn_padded = warn_padded;
  warn_padded = 0;

  for (unsigned i = 0; i < maxclusters; i++)
    {
      TYPE_FIELDS (newtype[i]) = newfields[i];
      layout_type (newtype[i]);
    }

  if (current_fc_level == fc_level::DYNAMIC)
    {
      gcc_assert (maxclusters == 1);
      fc_info->variant->new_type = newtype[0];
    }

  warn_padded = save_warn_padded;

  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS
      && replace_type_map.get (this->newtype[0]) == NULL)
    replace_type_map.put (this->newtype[0], this->type);
  if (dump_file)
    {
      if (current_layout_opt_level & DEAD_FIELD_ELIMINATION
	  && has_dead_field ())
	fprintf (dump_file, "Dead field elimination.\n");
    }

  if (pc_candidate && pc_gptr == NULL_TREE)
    create_global_ptr_for_pc ();

  if (semi_relayout)
    {
      bucket_size = calculate_bucket_size ();
      if (bucket_size == 0)
	return false;
      if (semi_relayout_map.get (this->newtype[0]) == NULL)
	semi_relayout_map.put (this->newtype[0], this->type);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Created %d types:\n", maxclusters);
      for (unsigned i = 0; i < maxclusters; i++)
	{
	  print_generic_expr (dump_file, newtype[i]);
	  fprintf (dump_file, "(%d)", TYPE_UID (newtype[i]));
	  fprintf (dump_file, "\n");
	}
    }

  return true;
}

/* Helper function to copy some attributes from ORIG_DECL to the NEW_DECL.  */

static inline void
copy_var_attributes (tree new_decl, tree orig_decl)
{
  DECL_ARTIFICIAL (new_decl) = 1;
  DECL_EXTERNAL (new_decl) = DECL_EXTERNAL (orig_decl);
  TREE_STATIC (new_decl) = TREE_STATIC (orig_decl);
  TREE_PUBLIC (new_decl) = TREE_PUBLIC (orig_decl);
  TREE_USED (new_decl) = TREE_USED (orig_decl);
  DECL_CONTEXT (new_decl) = DECL_CONTEXT (orig_decl);
  TREE_THIS_VOLATILE (new_decl) = TREE_THIS_VOLATILE (orig_decl);
  TREE_ADDRESSABLE (new_decl) = TREE_ADDRESSABLE (orig_decl);
  TREE_READONLY (new_decl) = TREE_READONLY (orig_decl);
  if (is_global_var (orig_decl))
    set_decl_tls_model (new_decl, DECL_TLS_MODEL (orig_decl));
}

/* Create all of the new decls (SSA_NAMES included) for THIS function.  */

void
srfunction::create_new_decls (void)
{
  /* If this function has been cloned, we don't need to
     create the new decls.  */
  if (newnode)
    return;

  if (node)
    push_cfun (DECL_STRUCT_FUNCTION (node->decl));

  for (unsigned i = 0; i < decls.length (); i++)
    {
      srdecl *decl = decls[i];
      srtype *type = decl->type;
      /* If the type of the decl does not change,
	 then don't create a new decl.  */
      if (!type->has_new_type ())
	{
	  decl->newdecl[0] = decl->decl;
	  continue;
	}

      /* Handle SSA_NAMEs.  */
      if (TREE_CODE (decl->decl) == SSA_NAME)
	{
	  tree newtype1[max_split];
	  tree inner = SSA_NAME_VAR (decl->decl);
	  tree newinner[max_split];
	  memset (newinner, 0, sizeof (newinner));
	  for (unsigned j = 0; j < max_split && type->newtype[j]; j++)
	    {
	      newtype1[j] = reconstruct_complex_type (
		      isptrptr (decls[i]->orig_type) ? decls[i]->orig_type
		      : TREE_TYPE (decls[i]->decl),
		      type->newtype[j]);
	    }
	  if (inner)
	    {
	      srdecl *in = find_decl (inner);
	      gcc_assert (in);
	      memcpy (newinner, in->newdecl, sizeof (newinner));
	    }
	  tree od = decls[i]->decl;
	  /* Create the new ssa names and copy some attributes
	     from the old one.  */
	  for (unsigned j = 0; j < max_split && type->newtype[j]; j++)
	    {
	      tree nd = make_ssa_name (newinner[j] ? newinner[j]
						   : newtype1[j]);
	      decl->newdecl[j] = nd;
	      /* If the old decl was a default definition,
		 handle it specially.  */
	      if (SSA_NAME_IS_DEFAULT_DEF (od))
		{
		  SSA_NAME_IS_DEFAULT_DEF (nd) = true;
		  SSA_NAME_DEF_STMT (nd) = gimple_build_nop ();

		  /* Set the default definition for the ssaname if needed.  */
		  if (inner)
		    {
		      gcc_assert (newinner[j]);
		      set_ssa_default_def (cfun, newinner[j], nd);
		    }
		}
	      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (nd)
		= SSA_NAME_OCCURS_IN_ABNORMAL_PHI (od);
	      statistics_counter_event (cfun, "Create new ssa_name", 1);
	    }
	}
      else if (TREE_CODE (decls[i]->decl) == VAR_DECL)
	{
	  tree orig_var = decl->decl;
	  const char *tname = NULL;
	  if (DECL_NAME (orig_var))
	    tname = IDENTIFIER_POINTER (DECL_NAME (orig_var));
	  for (unsigned j = 0; j < max_split && type->newtype[j]; j++)
	    {
	      tree new_name = NULL;
	      char *name = NULL;
	      char id[10];
	      sprintf (id, "%d", j);
	      if (tname)
		{
		  name = concat (tname, ".reorg.", id, NULL);
		  new_name = get_identifier (name);
		  free (name);
		}
	      tree newtype1 = reconstruct_complex_type (TREE_TYPE (orig_var),
							type->newtype[j]);
	      decl->newdecl[j] = build_decl (DECL_SOURCE_LOCATION (orig_var),
					     VAR_DECL, new_name, newtype1);
	      copy_var_attributes (decl->newdecl[j], orig_var);
	      if (!is_global_var (orig_var))
		add_local_decl (cfun, decl->newdecl[j]);
	      else
		varpool_node::add (decl->newdecl[j]);
	      statistics_counter_event (cfun, "Create new var decl", 1);
	    }
	}
      /* Paramater decls are already handled in create_new_functions.  */
      else if (TREE_CODE (decls[i]->decl) == PARM_DECL)
	;
      else
	internal_error ("Unhandled declaration type stored");

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Created New decls for decl:\n");
	  decls[i]->dump (dump_file);
	  fprintf (dump_file, "\n");
	  for (unsigned j = 0; j < max_split && decls[i]->newdecl[j]; j++)
	    {
	      print_generic_expr (dump_file, decls[i]->newdecl[j]);
	      fprintf (dump_file, "\n");
	    }
	  fprintf (dump_file, "\n");
	}
    }

  if (node)
    pop_cfun ();
}

/* Dump out the field structure to FILE.  */

void
srfield::dump (FILE *f)
{
  fprintf (f, "field (%d) { ", DECL_UID (fielddecl));
  fprintf (f, "base = ");
  base->simple_dump (f);
  fprintf (f, ", offset = " HOST_WIDE_INT_PRINT_DEC, offset);
  fprintf (f, ", type = ");
  print_generic_expr (f, fieldtype);
  fprintf (f, "}\n");
}

/* A simplified dump out the field structure to FILE.  */

void
srfield::simple_dump (FILE *f)
{
  if (fielddecl)
    fprintf (f, "field (%d)", DECL_UID (fielddecl));
}

sraccess::sraccess (tree e, gimple *s, cgraph_node *n, srfunction *srfn,
		    srtype *t, tree b, srfield *f)
    : expr (e),
      stmt (s),
      node (n),
      function (srfn),
      type (t),
      base (b),
      field (f)
{
  for (unsigned i = 0; i < gimple_num_ops (stmt); i++)
    {
      if (gimple_op (stmt, i) == expr)
	{
	  index = i;
	  return;
	}
    }

  gcc_unreachable ();
}

/* Dump out the access structure to FILE.  */

void
sraccess::dump (FILE *f) const
{
  fprintf (f, "access { ");
  fprintf (f, "type = '(");
  type->simple_dump (f);
  fprintf (f, ")'");
  if (field)
    {
      fprintf (f, ", field = '(");
      field->simple_dump (f);
      fprintf (f, ")'");
    }
  else
    fprintf (f, ", whole type");
  fprintf (f, " in function: %s/%d", node->name (), node->order);
  fprintf (f, ", stmt:\n");
  print_gimple_stmt (f, stmt, 0);
  fprintf (f, "}\n");
}

/* Check if it's an assignment to the given field(fielddecl != NULL_TREE)
   or any field(fielddecl == NULL_TREE).  */

bool
sraccess::write_field_p (tree fielddecl) const
{
  return write_p () && field && (!fielddecl || field->fielddecl == fielddecl);
}

/* Check if it's an assignment that read the given
   field(fielddecl != NULL_TREE) or any field(fielddecl == NULL_TREE).  */

bool
sraccess::read_field_p (tree fielddecl) const
{
  return read_p () && field && (!fielddecl || field->fielddecl == fielddecl);
}

bool
sraccess::write_p () const
{
  return is_gimple_assign (stmt) && index == 0;
}

bool
sraccess::read_p () const
{
  return is_gimple_assign (stmt) && index > 0;
}

/* Dump out the decl structure to FILE.  */

void
srdecl::dump (FILE *file)
{
  if (!func)
    fprintf (file, "global ");
  if (argumentnum != -1)
    fprintf (file, "argument(%d) ", argumentnum);
  fprintf (file, "decl: ");
  print_generic_expr (file, decl);
  fprintf (file, " type: ");
  type->simple_dump (file);
}

void
fc_closure::add_read_change (gimple *stmt)
{
  if (!read_change_set.contains (stmt))
    read_change_set.add (stmt);
}

bool
fc_closure::read_change_p (gimple *stmt)
{
  return read_change_set.contains (stmt);
}

void
fc_closure::add_read_unchange (gimple *stmt)
{
  if (!read_unchange_set.contains (stmt))
    read_unchange_set.add (stmt);
}

bool
fc_closure::read_unchange_p (gimple *stmt)
{
  return read_unchange_set.contains (stmt);
}

void
fc_closure::add_write_change (gimple *stmt)
{
  if (!write_change_set.contains (stmt))
    write_change_set.add (stmt);
}

bool
fc_closure::write_change_p (gimple *stmt)
{
  return write_change_set.contains (stmt);
}

void
fc_closure::add_write_unchange (gimple *stmt)
{
  if (!write_unchange_set.contains (stmt))
    write_unchange_set.add (stmt);
}

bool
fc_closure::write_unchange_p (gimple *stmt)
{
  return write_unchange_set.contains (stmt);
}

bool
fc_closure::change_p (gimple *stmt)
{
  return write_change_p (stmt) || read_change_p (stmt);
}

bool
fc_closure::unchange_p (gimple *stmt)
{
  return write_unchange_p (stmt) || read_unchange_p (stmt);
}

/* Call compress/decompress function for rhs.  */

tree
fc_closure::convert_rhs (tree rhs, tree fn)
{
  tree newrhs = build_call_expr (fn, 1, rhs);
  cgraph_node *callee = cgraph_node::get (fn);
  cgraph_node *node = cgraph_node::get (current_function_decl);
  node->create_edge (callee, NULL, profile_count::zero ());

  return newrhs;
}

void
closure_helper::record_origin_closure (basic_block bb)
{
  for (auto si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      if (!is_gimple_assign (stmt))
	continue;

      uid++;

      if (cinfo->read_change_p (stmt))
	bitmap_set_bit (read_change_map, uid);
      else if (cinfo->write_change_p (stmt))
	bitmap_set_bit (write_change_map, uid);
      else if (cinfo->read_unchange_p (stmt))
	bitmap_set_bit (read_unchange_map, uid);
      else if (cinfo->write_unchange_p (stmt))
	bitmap_set_bit (write_unchange_map, uid);
    }
}

void
closure_helper::add_cloned_closure (basic_block bb)
{
  for (auto si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      if (!is_gimple_assign (stmt))
	continue;

      uid++;

      if (bitmap_bit_p (read_change_map, uid))
	cinfo->add_read_change (stmt);
      else if (bitmap_bit_p (write_change_map, uid))
	cinfo->add_write_change (stmt);
      else if (bitmap_bit_p (read_unchange_map, uid))
	cinfo->add_read_unchange (stmt);
      else if (bitmap_bit_p (write_unchange_map, uid))
	cinfo->add_write_unchange (stmt);
    }
}

void
closure_helper::reset_uid ()
{
  uid = 0;
}

void
fc_field_class::dump (FILE *file) const
{
  fprintf (file, "field type: ");
  print_generic_expr (file, fieldtype);
  fprintf (file, "\n");
  fprintf (file, "fields: ");

  unsigned i;
  srfield *srf;
  FOR_EACH_VEC_ELT (srfields, i, srf)
    {
      print_generic_expr (file, srf->fielddecl);
      if (i == srfields.length () - 1)
	fprintf (file, "\n");
      else
	fprintf (file, ", ");
    }
}

unsigned
fc_field_class::size () const
{
  return srfields.length ();
}

/* Search and return the index of the given srfield.
   Return -1 if couldn't find the srfield.  */

int
fc_field_class::get_field_index (srfield *field) const
{
  unsigned i;
  srfield *srf;
  FOR_EACH_VEC_ELT (srfields, i, srf)
    if (srf == field)
      return i;

  return -1;
}

void
fc_ref::dump (FILE *file) const
{
  fprintf (file, "var: ");
  print_generic_expr (dump_file, var);
  fprintf (dump_file, ", type: ");
  print_generic_expr (dump_file, orig_type ? orig_type : TREE_TYPE (var));
  fprintf (dump_file, ", array: ");
  print_generic_expr (dump_file, source->var);
  if (size)
    {
      fprintf (dump_file, ", array size: ");
      print_generic_expr (dump_file, size);
    }
  if (field)
    {
      fprintf (dump_file, ", field: ");
      print_generic_expr (dump_file, field);
    }
  fprintf (dump_file, "\n");
}

fc_type_info::~fc_type_info ()
{
  if (variant)
    {
      delete variant;
      variant = NULL;
    }
}

fc_field_class *
fc_type_info::find_field_class_by_type (tree type) const
{
  for (auto *field_class : field_classes)
    {
      if (field_class->fieldtype == type)
	return field_class;
    }

  return NULL;
}

fc_field_class *
fc_type_info::record_field_class (srfield *srf)
{
  if (srf->field_class)
    return srf->field_class;

  fc_field_class *field_class = find_field_class_by_type (srf->fieldtype);

  if (!field_class)
    {
      field_class = new fc_field_class (srf->fieldtype);
      field_classes.safe_push (field_class);
    }

  srf->field_class = field_class;
  field_class->srfields.safe_push (srf);

  return field_class;
}

fc_cond *
fc_type_info::find_cond (tree type) const
{
  for (auto *cond : fc_conds)
    {
      if (cond->old_type == type)
	return cond;
    }

  return NULL;
}

fc_cond *
fc_type_info::create_cond (tree type)
{
  fc_cond *cond = find_cond (type);
  if (cond)
    return cond;

  /* New cond will be stored in an auto_delete_vec(fc_conds).  */
  cond = new fc_cond (type);
  fc_conds.safe_push (cond);

  /* Record the fc_cond to corresponding fc_field_class.  */
  fc_field_class *field_class = find_field_class_by_type (type);
  gcc_assert (field_class);
  field_class->cond = cond;
  cond->field_class = field_class;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Create new fc_cond, type: ");
      print_generic_expr (dump_file, type);
      fprintf (dump_file, "\n");
    }

  return cond;
}

void
fc_type_info::record_cond (fc_field *fc_f)
{
  if (fc_f->cond)
    return;

  fc_cond *cond = create_cond (TREE_TYPE (fc_f->field));
  fc_f->cond = cond;
  cond->fields.safe_push (fc_f);
}

fc_path_info::~fc_path_info ()
{
  if (cloned_func)
    delete cloned_func;
}

/* Search and store basic_blocks that:
   1) can reach STMT (when DIRECTION == PRED);
   2) can be reached from STMT (when DIRECTION == SUCC).
   Return false if field compression cannot be performed.  */

bool
fc_path_info::collect_blocks (gimple *stmt, direction dir)
{
  basic_block start_bb = gimple_bb (stmt);
  if (!start_bb)
    return false;

  /* The start block should not be in a loop.  */
  if (start_bb->loop_father != NULL
      && loop_outer (start_bb->loop_father) != NULL)
    return false;

  bool prev = dir == direction::PRED;
  basic_block stop_bb = prev ? ENTRY_BLOCK_PTR_FOR_FN (cfun)
			     : EXIT_BLOCK_PTR_FOR_FN (cfun);
  auto *store_list = prev ? &pre_bbs : &reach_bbs;

  auto_bitmap visited;
  auto_vec<basic_block> worklist;
  worklist.safe_push (start_bb);
  bool exit_p = false;

  while (!worklist.is_empty ())
    {
      basic_block bb = worklist.pop ();
      if (!bitmap_set_bit (visited, bb->index))
	continue;

      if (bb != stop_bb)
	store_list->safe_push (bb);
      else
	exit_p = true;

      if (prev)
	for (auto *e : bb->preds)
	  worklist.safe_push (e->src);
      else
	for (auto *e : bb->succs)
	  worklist.safe_push (e->dest);
    }

  if (!exit_p)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Found %d blocks in func ", store_list->length ());
      print_generic_expr (dump_file, cfun->decl);
      fprintf (dump_file, " %s:\n",
	       prev ? "before start-point" : "to be cloned");
      for (auto *bb : store_list)
	fprintf (dump_file, "%d ", bb->index);
      fprintf (dump_file, "\n\n");
    }

  return true;
}

} // namespace struct_reorg


namespace struct_relayout {

/* Complete Structure Relayout Optimization.
   It reorganizes all structure members, and puts same member together.
   struct s {
     long a;
     int b;
     struct s *c;
   };
   Array looks like
     abcabcabcabc...
   will be transformed to
     aaaa...bbbb...cccc...
*/

#define GPTR_SIZE(i) \
  TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (gptr[i])))

unsigned transformed = 0;

unsigned
csrtype::calculate_field_num (tree field_offset)
{
  if (field_offset == NULL)
    return 0;

  HOST_WIDE_INT off = int_byte_position (field_offset);
  unsigned i = 1;
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (off == int_byte_position (field))
	return i;
      i++;
    }
  return 0;
}

void
csrtype::init_type_info (void)
{
  if (!type)
    return;
  new_size = old_size = tree_to_uhwi (TYPE_SIZE_UNIT (type));

  /* Close enough to pad to improve performance.
     33~63 should pad to 64 but 33~48 (first half) are too far away, and
     70~127 should pad to 128 but 65~70 (first half) are too far away.  */
  if (old_size > 48 && old_size < 64)
    new_size = 64;
  if (old_size > 70 && old_size < 128)
    new_size = 128;

  /* For performance reasons, only allow structure size
     that is a power of 2 and not too big.  */
  if (new_size != 1 && new_size != 2
      && new_size != 4 && new_size != 8
      && new_size != 16 && new_size != 32
      && new_size != 64 && new_size != 128)
    {
      new_size = 0;
      field_count = 0;
      return;
    }

  unsigned i = 0;
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL)
      i++;
  field_count = i;

  struct_size = build_int_cstu (TREE_TYPE (TYPE_SIZE_UNIT (type)),
				new_size);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Type: ");
      print_generic_expr (dump_file, type);
      fprintf (dump_file, " has %d members.\n", field_count);
      fprintf (dump_file, "Modify struct size from %ld to %ld.\n",
			  old_size, new_size);
    }
}

} // namespace struct_relayout


namespace {

struct const_map
{
  tree var;
  HOST_WIDE_INT value;
  const_map (tree var, HOST_WIDE_INT value)
    : var (var), value (value)
  {}
};

struct ipa_struct_reorg
{
private:
  /* The current srfield set in rewrite_expr.  For dfc.  */
  srfield *cur_srfd;

  auto_vec_del<const_map> global_consts;
  hash_set<tree> visited_vars;

public:
  // Constructors
  ipa_struct_reorg (void)
    : done_recording (false)
  {}

  // Fields
  auto_vec_del<srtype> types;
  auto_vec_del<srfunction> functions;
  srglobal globals;
  hash_set <cgraph_node *> safe_functions;
  auto_vec<srtype *> ext_func_types;
  auto_vec_del<fc_type_info> fc_infos;
  auto_vec <tree> release_ssa_names;

  bool done_recording;

  // Methods
  unsigned execute (unsigned int opt);
  void mark_type_as_escape (tree type, escape_type escapes,
			    gimple *stmt = NULL);

  void dump_types (FILE *f);
  void dump_newtypes (FILE *f);
  void dump_types_escaped (FILE *f);
  void dump_functions (FILE *f);
  void record_accesses (void);
  void detect_cycles (void);
  bool walk_field_for_cycles (srtype *);
  void prune_escaped_types (void);
  void prune_function (srfunction *);
  void prune_globals ();
  void propagate_escape (void);
  void propagate_escape_via_original (void);
  void propagate_escape_via_empty_with_no_original (void);
  void propagate_escape_via_ext_func_types (void);
  void propagate_escape_via_no_record_var (void);
  void analyze_types (void);
  void clear_visited (void);
  bool create_new_types (void);
  void create_new_decls (void);
  srdecl *find_decl (tree);
  void create_new_functions (void);
  void create_new_args (cgraph_node *new_node);
  unsigned rewrite_functions (void);
  void rewrite_block (basic_block);
  srdecl *record_var (tree decl,
		      escape_type escapes = does_not_escape,
		      int arg = -1);
  void record_safe_func_with_void_ptr_parm (void);
  srfunction *record_function (cgraph_node *node, srfunction *sfn = NULL);
  srfunction *find_function (cgraph_node *node);
  void record_field_type (tree field, srtype *base_srtype);
  void record_struct_field_types (tree base_type, srtype *base_srtype);
  srtype *record_type (tree type);
  void process_union (tree type);
  srtype *find_type (tree type);
  void maybe_record_stmt (cgraph_node *, gimple *);
  void maybe_record_assign (cgraph_node *, gassign *);
  void maybe_record_call (cgraph_node *, gcall *);
  void maybe_record_allocation_site (cgraph_node *, gimple *);
  void record_stmt_expr (tree expr, cgraph_node *node, gimple *stmt);
  void mark_expr_escape (tree, escape_type, gimple *stmt);
  bool handled_allocation_stmt (gimple *stmt);
  tree allocate_size (srtype *t, srdecl *decl, gimple *stmt);

  void mark_decls_in_as_not_needed (tree fn);

  bool rewrite_stmt (gimple *, gimple_stmt_iterator *);
  bool rewrite_assign (gassign *, gimple_stmt_iterator *);
  bool rewrite_call (gcall *, gimple_stmt_iterator *);
  bool rewrite_cond (gcond *, gimple_stmt_iterator *);
  bool rewrite_debug (gimple *, gimple_stmt_iterator *);
  bool rewrite_phi (gphi *);
  bool rewrite_expr (tree expr,
		     tree newexpr[max_split],
		     bool ignore_missing_decl = false);
  bool rewrite_lhs_rhs (tree lhs, tree rhs, tree newlhs[max_split],
			tree newrhs[max_split]);
  bool get_type_field (tree expr, tree &base, bool &indirect, srtype *&type,
		       srfield *&field, bool &realpart, bool &imagpart,
		       bool &address, bool &escape_from_base,
		       bool should_create = false, bool can_escape = false);
  bool wholeaccess (tree expr, tree base, tree accesstype, srtype *t);

  void check_alloc_num (gimple *stmt, srtype *type, bool ptrptr);
  void check_definition_assign (srdecl *decl, vec<srdecl *> &worklist);
  void check_definition_call (srdecl *decl, vec<srdecl *> &worklist);
  void check_definition (srdecl *decl, vec<srdecl *> &);
  void check_uses (srdecl *decl, vec<srdecl *> &);
  void check_use (srdecl *decl, gimple *stmt, vec<srdecl *> &);
  void check_type_and_push (tree newdecl, srdecl *decl,
			    vec<srdecl *> &worklist, gimple *stmt);
  void check_other_side (srdecl *decl, tree other, gimple *stmt,
			 vec<srdecl *> &worklist);
  void check_ptr_layers (tree a_expr, tree b_expr, gimple *stmt);

  void find_vars (gimple *stmt);
  void find_var (tree expr, gimple *stmt);
  void mark_types_asm (gasm *astmt);

  bool has_rewritten_type (srfunction *);
  void maybe_mark_or_record_other_side (tree side, tree other, gimple *stmt);

  unsigned execute_struct_relayout (void);
  bool remove_dead_field_stmt (tree lhs);

  // Pointer compression methods:
  void check_and_prune_struct_for_pointer_compression (void);
  void try_rewrite_with_pointer_compression (gassign *, gimple_stmt_iterator *,
					     tree, tree, tree &, tree &);
  bool safe_void_cmp_p (tree, srtype *);
  bool pc_candidate_st_type_p (tree);
  bool pc_candidate_tree_p (tree);
  bool pc_type_conversion_candidate_p (tree);
  bool pc_direct_rewrite_chance_p (tree, tree &);
  bool pc_simplify_chance_for_compress_p (gassign *, tree);
  bool compress_candidate_without_check (gimple_stmt_iterator *, tree, tree &);
  bool compress_candidate_with_check (gimple_stmt_iterator *, tree, tree &);
  bool compress_candidate (gassign *, gimple_stmt_iterator *, tree, tree &);
  bool decompress_candidate_without_check (gimple_stmt_iterator *,
					   tree, tree, tree &, tree &);
  bool decompress_candidate_with_check (gimple_stmt_iterator *, tree, tree &);
  bool decompress_candidate (gimple_stmt_iterator *, tree, tree, tree &,
			     tree &);
  srtype *get_compression_candidate_type (tree);
  tree compress_ptr_to_offset (tree, srtype *, gimple_stmt_iterator *);
  tree decompress_offset_to_ptr (tree, srtype *, gimple_stmt_iterator *);
  basic_block create_bb_for_compress_candidate (basic_block, tree, srtype *,
						tree &);
  basic_block create_bb_for_decompress_candidate (basic_block, tree, srtype *,
						  tree &);
  basic_block create_bb_for_compress_nullptr (basic_block, tree &);
  basic_block create_bb_for_decompress_nullptr (basic_block, tree, tree &);

   // Semi-relayout methods:
  bool is_semi_relayout_candidate (tree);
  srtype *get_semi_relayout_candidate_type (tree);
  void check_and_prune_struct_for_semi_relayout (void);
  tree rewrite_pointer_diff (gimple_stmt_iterator *, tree, tree, srtype *);
  tree rewrite_pointer_plus_integer (gimple *, gimple_stmt_iterator *, tree,
				     tree, srtype *);
  tree build_div_expr (gimple_stmt_iterator *, tree, tree);
  tree get_true_pointer_base (gimple_stmt_iterator *, tree, srtype *);
  tree get_real_allocated_ptr (tree, gimple_stmt_iterator *);
  tree set_ptr_for_use (tree, gimple_stmt_iterator *);
  void record_allocated_size (tree, gimple_stmt_iterator *, tree);
  tree read_allocated_size (tree, gimple_stmt_iterator *);
  gimple *create_aligned_alloc (gimple_stmt_iterator *, srtype *, tree,
				tree &);
  void create_memset_zero (tree, gimple_stmt_iterator *, tree);
  void create_memcpy (tree, tree, tree, gimple_stmt_iterator *);
  void create_free (tree, gimple_stmt_iterator *);
  void copy_to_lhs (tree, tree, gimple_stmt_iterator *);
  srtype *get_relayout_candidate_type (tree);
  long unsigned int get_true_field_offset (srfield *, srtype *);
  tree rewrite_address (tree, srfield *, srtype *, gimple_stmt_iterator *);
  bool check_sr_copy (gimple *);
  void relayout_field_copy (gimple_stmt_iterator *, gimple *, tree, tree,
			    tree&, tree &);
  bool do_semi_relayout (gimple_stmt_iterator *, gimple *, tree &, tree &);

  // field-compress methods:
  bool get_base_type (tree, tree &, srtype *&, srfield *&);
  void check_and_prune_struct_for_field_compression ();
  bool find_field_compression_candidate (srtype *);
  void classify_fields (fc_type_info *);
  bool find_static_fc_fields (fc_type_info *);
  bool compress_fields (fc_type_info *);
  bool find_shadow_fields (fc_type_info *);
  bool find_shadow_fields (fc_type_info *, fc_field_class *);
  bool find_pair_stmts (fc_field_class *, fc_shadow_info &);
  void add_pair_stmts_group (fc_shadow_info &,
			     const auto_vec<gimple *> &,
			     const auto_vec<unsigned> &);
  srfield *read_field_in_fc_class_p (gimple *, fc_field_class *);
  srfield *write_field_in_fc_class_p (gimple *, fc_field_class *);
  bool fc_pair_stmts_rhs_equal_p (const auto_vec<gimple *> &);
  bool unique_init_const_p (const fc_shadow_info &);
  bool fc_operand_equal_p (tree, tree);
  bool fc_global_const_p (tree, HOST_WIDE_INT &);
  bool fc_peephole_const_p (tree, HOST_WIDE_INT &);
  const_map *find_global_const (tree);
  bool check_unpair_stmt (fc_field_class *, gimple *,
			  srfunction *, srfield *);
  tree find_mem_base (tree);
  gimple *find_alloc_stmt (tree);
  bool static_compress_p (fc_type_info *, tree);
  HOST_WIDE_INT find_max_value (srtype *, tree);
  std::pair<bool, HOST_WIDE_INT> find_assign_max_value (gimple *);
  bool struct_copy_p (gimple *, tree);
  bool find_hot_access (fc_type_info *, auto_vec<fc_field *> &);
  void cleanup_shadow_write (fc_type_info *);
  void rewrite_shadow_read (fc_type_info *);
  void modify_shadow_read (gimple *, unsigned, fc_field *, tree);
  bool compress_fields_static (fc_type_info *info);
  bool compress_to_bitfield_static (fc_type_info *info);
  bool compress_to_bitfield_dynamic (fc_type_info *info);
  auto_vec<basic_block> collect_all_predecessor (gimple *);
  bool types_fc_equal_p (tree, tree);
  bool types_fc_compatible_p (tree, tree);
  bool find_dynamic_fc_fields (fc_type_info *);
  bool find_fields_in_input_stmt (fc_type_info *);
  bool find_input_stmt (gimple *, gimple *&, gimple *&);
  tree find_file_handler (gimple *);
  bool find_fopen_fclose (fc_type_info *);
  bool check_dynamic_shadow_fields (fc_type_info *);
  bool find_fc_paths (fc_type_info *);
  bool find_fc_data (fc_type_info *);
  bool find_fc_arrays (fc_type_info *);
  bool find_fc_array (fc_type_info *, tree, varpool_node *);
  bool duplicative_array_p (fc_type_info *, tree);
  bool is_stmt_before_fclose (fc_type_info *, gimple *, symtab_node *);
  bool reorg_ptr_p (tree);
  bool get_allocate_size_iterate (tree, gimple *, tree &, tree * = NULL);
  bool get_allocate_size_assign (tree, gassign *, tree &, tree *);
  bool get_allocate_size_call (tree, gcall *, tree &, tree *);
  bool get_allocate_size_reorg_ptr (gimple *, tree &);
  tree get_allocate_size (tree, tree, tree, gimple *);
  bool find_fc_refs (fc_type_info *);
  bool find_fc_refs_iterate (fc_type_info *, fc_array *, tree, bool);
  bool find_fc_refs_ssa_name (fc_type_info *, fc_array *, tree, bool);
  bool find_fc_refs_mem_ref (fc_type_info *, fc_array *, tree);
  bool find_fc_refs_component_ref (fc_type_info *, fc_array *, tree);
  bool fc_type_pointer_p (fc_type_info *, tree);
  bool add_fc_ref (fc_type_info *, fc_array *, tree, tree);
  check_ref_result check_duplicative_ref (fc_type_info *, fc_array *, tree,
  					  tree, tree &, tree &);
  gimple *find_def_stmt_before_fclose (fc_type_info *, tree);
  tree get_ptr_decl (tree);
  bool check_fc_array_uses (fc_type_info *);
  void calc_fc_ref_count (fc_type_info *);
  bool compress_fields_dynamic (fc_type_info *);
  bool calc_dynamic_boundary (fc_type_info *);
  bool fc_cond_field_p (tree, const fc_cond *);
  bool fc_input_ssa_p (tree, const fc_cond *);
  bool fc_field_load_p (tree, const fc_cond *);
  void update_high_bound (fc_cond *, HOST_WIDE_INT);
  bool check_closure (fc_type_info *);
  bool check_closure (fc_type_info *, fc_cond *);
  bool write_field_class_only_p (fc_type_info *, fc_field_class *, tree);
  void collect_closure_read_change (fc_type_info *, fc_field_class *);
  unsigned execute_dynamic_field_compression ();
  unsigned dynamic_fc_rewrite ();
  bool create_dynamic_fc_newtypes ();
  void create_dynamic_fc_variant (fc_type_info *);
  void create_global_var_dfc_path (fc_type_info *);
  void create_dynamic_fc_convert_fn (fc_type_info *);
  tree create_convert_fn (fc_cond *, unsigned, bool);
  edge create_normal_part (fc_cond *);
  void create_conversion_part (fc_cond *, edge, bool);
  void clone_dynamic_fc_path (fc_type_info *);
  void clone_partial_func (fc_type_info *, srfunction *);
  void clone_whole_func (srfunction *);
  void rewrite_dynamic_shadow_fields (fc_type_info *);
  void rewrite_dynamic_fc_path ();
  void record_dfc_path_info (fc_type_info *);
  void collect_closure_info_dynamic (fc_type_info *);
  void collect_closure_info_partial (srfunction *, fc_closure *);
  void collect_closure_info_whole (srfunction *, fc_closure *);
  void rewrite_partial_func (srfunction *);
  void rewrite_whole_func (srfunction *);
  void clean_func_after_rewrite (srfunction *);
  void dynamic_fc_rewrite_assign (gimple *, tree, tree &, tree &);
  void add_dynamic_checking (fc_type_info *);
  void insert_code_calc_dfc_path (fc_type_info *);
  void insert_code_calc_max_min_val (fc_type_info *);
  tree insert_code_calc_cond (fc_type_info *, gimple_stmt_iterator *);
  void insert_code_check_init_const (fc_type_info *, gimple_stmt_iterator *,
				     tree &);
  void insert_code_compress_data (fc_type_info *, edge);
  void insert_code_compress_variant (fc_type_info *, basic_block,
				     const auto_vec<tree> &,
				     const auto_vec<tree> &);
  void insert_code_compress_array (fc_type_info *, edge &,
				   const auto_vec<tree> &,
				   const auto_vec<tree> &);
  void insert_code_modify_refs (fc_type_info *, edge);
  void create_compress_object_fn (fc_type_info *);
  edge insert_code_modify_single_ref (edge, tree, fc_array *, tree, tree);
};

struct ipa_struct_relayout
{
public:
  // Fields
  tree gptr[max_relayout_split + 1];
  csrtype ctype;
  ipa_struct_reorg *sr;
  cgraph_node *current_node;

  // Constructors
  ipa_struct_relayout (tree type, ipa_struct_reorg *sr_)
  {
    ctype.type = type;
    sr = sr_;
    current_node = NULL;
    for (int i = 0; i < max_relayout_split + 1; i++)
      gptr[i] = NULL;
  }

  // Methods
  tree create_new_vars (tree type, const char *name);
  void create_global_ptrs (void);
  unsigned int rewrite (void);
  void rewrite_stmt_in_function (void);
  bool rewrite_debug (gimple *stmt, gimple_stmt_iterator *gsi);
  bool rewrite_stmt (gimple *stmt, gimple_stmt_iterator *gsi);
  bool handled_allocation_stmt (gcall *stmt);
  void init_global_ptrs (gcall *stmt, gimple_stmt_iterator *gsi);
  bool check_call_uses (gcall *stmt);
  bool rewrite_call (gcall *stmt, gimple_stmt_iterator *gsi);
  tree create_ssa (tree node, gimple_stmt_iterator *gsi);
  bool is_candidate (tree xhs);
  tree rewrite_address (tree xhs, gimple_stmt_iterator *gsi);
  tree rewrite_offset (tree offset, HOST_WIDE_INT num);
  bool rewrite_assign (gassign *stmt, gimple_stmt_iterator *gsi);
  bool maybe_rewrite_cst (tree cst, gimple_stmt_iterator *gsi,
			  HOST_WIDE_INT &times);
  unsigned int execute (void);
};

} // anon namespace

namespace {

/* Methods for ipa_struct_relayout.  */

tree
ipa_struct_relayout::create_new_vars (tree type, const char *name)
{
  gcc_assert (type);
  tree new_type = build_pointer_type (type);

  tree new_name = NULL;
  if (name)
    new_name = get_identifier (name);

  tree new_var = build_decl (UNKNOWN_LOCATION, VAR_DECL, new_name, new_type);

  /* Set new_var's attributes.  */
  set_var_attributes (new_var);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Created new var: ");
      print_generic_expr (dump_file, new_var);
      fprintf (dump_file, "\n");
    }
  return new_var;
}

void
ipa_struct_relayout::create_global_ptrs (void)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Create global gptrs: {\n");

  char *gptr0_name = NULL;
  const char *type_name = get_type_name (ctype.type);

  if (type_name)
    gptr0_name = concat (type_name, "_gptr0", NULL);
  tree var_gptr0 = create_new_vars (ctype.type, gptr0_name);
  gptr[0] = var_gptr0;
  varpool_node::add (var_gptr0);

  unsigned i = 1;
  for (tree field = TYPE_FIELDS (ctype.type); field;
       field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL)
	{
	  tree type = TREE_TYPE (field);

	  char *name = NULL;
	  char id[10] = {0};
	  sprintf (id, "%d", i);
	  const char *decl_name = IDENTIFIER_POINTER (DECL_NAME (field));

	  if (type_name && decl_name)
	    name = concat (type_name, "_", decl_name, "_gptr", id, NULL);
	  tree var = create_new_vars (type, name);

	  gptr[i] = var;
	  varpool_node::add (var);
	  i++;
	}
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nTotally create %d gptrs. }\n\n", i);
  gcc_assert (ctype.field_count == i - 1);
}

void
ipa_struct_relayout::rewrite_stmt_in_function (void)
{
  gcc_assert (cfun);

  basic_block bb = NULL;
  gimple_stmt_iterator si;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (si = gsi_start_bb (bb); !gsi_end_p (si);)
	{
	  gimple *stmt = gsi_stmt (si);
	  if (rewrite_stmt (stmt, &si))
	    gsi_remove (&si, true);
	  else
	    gsi_next (&si);
	}
    }

  /* Debug statements need to happen after all other statements
     have changed.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (si = gsi_start_bb (bb); !gsi_end_p (si);)
	{
	  gimple *stmt = gsi_stmt (si);
	  if (gimple_code (stmt) == GIMPLE_DEBUG
	      && rewrite_debug (stmt, &si))
	    gsi_remove (&si, true);
	  else
	    gsi_next (&si);
	}
    }
}

unsigned int
ipa_struct_relayout::rewrite (void)
{
  cgraph_node *cnode = NULL;
  function *fn = NULL;
  FOR_EACH_FUNCTION (cnode)
    {
      if (!cnode->real_symbol_p () || !cnode->has_gimple_body_p ())
	continue;
      if (cnode->definition)
	{
	  fn = DECL_STRUCT_FUNCTION (cnode->decl);
	  if (fn == NULL)
	    continue;

	  current_node = cnode;
	  push_cfun (fn);

	  rewrite_stmt_in_function ();

	  update_ssa (TODO_update_ssa_only_virtuals);

	  if (flag_tree_pta)
	    compute_may_aliases ();

	  remove_unused_locals ();

	  cgraph_edge::rebuild_edges ();

	  free_dominance_info (CDI_DOMINATORS);

	  pop_cfun ();
	  current_node = NULL;
	}
    }
  return TODO_verify_all;
}

bool
ipa_struct_relayout::rewrite_debug (gimple *stmt ATTRIBUTE_UNUSED,
				    gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED)
{
  /* Delete debug gimple now.  */
  return true;
}

bool
ipa_struct_relayout::rewrite_stmt (gimple *stmt, gimple_stmt_iterator *gsi)
{
  switch (gimple_code (stmt))
    {
      case GIMPLE_ASSIGN:
	return rewrite_assign (as_a <gassign *> (stmt), gsi);
      case GIMPLE_CALL:
	return rewrite_call (as_a <gcall *> (stmt), gsi);
      default:
	break;
    }
  return false;
}

bool
ipa_struct_relayout::handled_allocation_stmt (gcall *stmt)
{
  if (gimple_call_builtin_p (stmt, BUILT_IN_CALLOC))
    return true;
  return false;
}

void
ipa_struct_relayout::init_global_ptrs (gcall *stmt, gimple_stmt_iterator *gsi)
{
  gcc_assert (handled_allocation_stmt (stmt));

  tree lhs = gimple_call_lhs (stmt);

  /* Case that gimple is at the end of bb.  */
  if (gsi_one_before_end_p (*gsi))
    {
      gassign *gptr0 = gimple_build_assign (gptr[0], lhs);
      gsi_insert_after (gsi, gptr0, GSI_SAME_STMT);
    }
  gsi_next (gsi);

  /* Emit gimple gptr0 = _X and gptr1 = _X.  */
  gassign *gptr0 = gimple_build_assign (gptr[0], lhs);
  gsi_insert_before (gsi, gptr0, GSI_SAME_STMT);
  gassign *gptr1 = gimple_build_assign (gptr[1], lhs);
  gsi_insert_before (gsi, gptr1, GSI_SAME_STMT);

  /* Emit gimple gptr_[i] = gptr_[i-1] + _Y[gap].  */
  for (unsigned i = 2; i <= ctype.field_count; i++)
    {
      gimple *new_stmt = NULL;
      tree gptr_i_prev_ssa = create_ssa (gptr[i-1], gsi);
      tree gptr_i_ssa = make_ssa_name (TREE_TYPE (gptr[i-1]));

      /* Emit gimple _Y[gap] = N * sizeof (member).  */
      tree member_gap = gimplify_build2 (gsi, MULT_EXPR,
					 long_unsigned_type_node,
					 gimple_call_arg (stmt, 0),
					 GPTR_SIZE (i-1));

      new_stmt = gimple_build_assign (gptr_i_ssa, POINTER_PLUS_EXPR,
				      gptr_i_prev_ssa, member_gap);
      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);

      gassign *gptr_i = gimple_build_assign (gptr[i], gptr_i_ssa);
      gsi_insert_before (gsi, gptr_i, GSI_SAME_STMT);
    }
  gsi_prev (gsi);
}

bool
ipa_struct_relayout::check_call_uses (gcall *stmt)
{
  gcc_assert (current_node);
  srfunction *fn = sr->find_function (current_node);
  tree lhs = gimple_call_lhs (stmt);

  if (fn == NULL)
    return false;

  srdecl *d = fn->find_decl (lhs);
  if (d == NULL)
    return false;
  if (types_compatible_p (d->type->type, ctype.type))
    return true;

  return false;
}

bool
ipa_struct_relayout::rewrite_call (gcall *stmt, gimple_stmt_iterator *gsi)
{
  if (handled_allocation_stmt (stmt))
    {
      /* Rewrite stmt _X = calloc (N, sizeof (struct)).  */
      tree size = gimple_call_arg (stmt, 1);
      if (TREE_CODE (size) != INTEGER_CST)
	return false;
      if (tree_to_uhwi (size) != ctype.old_size)
	return false;
      if (!check_call_uses (stmt))
	return false;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Rewrite allocation call:\n");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "to\n");
	}

      /* Modify sizeof (struct).  */
      gimple_call_set_arg (stmt, 1, ctype.struct_size);
      update_stmt (stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}

      init_global_ptrs (stmt, gsi);
    }
  return false;
}

tree
ipa_struct_relayout::create_ssa (tree node, gimple_stmt_iterator *gsi)
{
  gcc_assert (TREE_CODE (node) == VAR_DECL);
  tree node_ssa = make_ssa_name (TREE_TYPE (node));
  gassign *stmt = gimple_build_assign (node_ssa, node);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
  return node_ssa;
}

bool
ipa_struct_relayout::is_candidate (tree xhs)
{
  if (TREE_CODE (xhs) != COMPONENT_REF)
    return false;
  tree mem = TREE_OPERAND (xhs, 0);
  if (TREE_CODE (mem) == MEM_REF)
    {
      tree type = TREE_TYPE (mem);
      if (types_compatible_p (type, ctype.type))
	return true;
    }
  return false;
}

tree
ipa_struct_relayout::rewrite_address (tree xhs, gimple_stmt_iterator *gsi)
{
  tree mem_ref = TREE_OPERAND (xhs, 0);
  tree pointer = TREE_OPERAND (mem_ref, 0);
  tree pointer_offset = TREE_OPERAND (mem_ref, 1);
  tree field = TREE_OPERAND (xhs, 1);

  tree pointer_ssa = fold_convert (long_unsigned_type_node, pointer);
  tree gptr0_ssa = fold_convert (long_unsigned_type_node, gptr[0]);

  /* Emit gimple _X1 = ptr - gptr0.  */
  tree step1 = gimplify_build2 (gsi, MINUS_EXPR, long_unsigned_type_node,
				pointer_ssa, gptr0_ssa);

  /* Emit gimple _X2 = _X1 / sizeof (struct).  */
  tree step2 = gimplify_build2 (gsi, TRUNC_DIV_EXPR, long_unsigned_type_node,
				step1, ctype.struct_size);

  unsigned field_num = ctype.calculate_field_num (field);
  gcc_assert (field_num > 0 && field_num <= ctype.field_count);

  /* Emit gimple _X3 = _X2 * sizeof (member).  */
  tree step3 = gimplify_build2 (gsi, MULT_EXPR, long_unsigned_type_node,
				step2, GPTR_SIZE (field_num));

  /* Emit gimple _X4 = gptr[I].  */
  tree gptr_field_ssa = create_ssa (gptr[field_num], gsi);
  tree new_address = make_ssa_name (TREE_TYPE (gptr[field_num]));
  tree new_address_type = TREE_TYPE (new_address);
  tree new_type = TREE_TYPE (new_address_type);
  gassign *new_stmt = gimple_build_assign (new_address, POINTER_PLUS_EXPR,
					   gptr_field_ssa, step3);
  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);

  /* MEM_REF with nonzero offset like
       MEM[ptr + sizeof (struct)] = 0B
     should be transformed to
       MEM[gptr + sizeof (member)] = 0B
  */
  tree new_size = NULL_TREE;
  if (integer_zerop (pointer_offset))
    new_size = build_int_cst (TREE_TYPE (new_address), 0);
  else
    {
      HOST_WIDE_INT size = tree_to_shwi (TYPE_SIZE_UNIT (new_type));
      new_size = rewrite_offset (pointer_offset, size);
    }

  if (new_size)
    TREE_OPERAND (mem_ref, 1) = new_size;

  /* Update mem_ref pointer.  */
  TREE_OPERAND (mem_ref, 0) = new_address;

  /* Update mem_ref TREE_TYPE.  */
  TREE_TYPE (mem_ref) = TREE_TYPE (TREE_TYPE (new_address));

  return mem_ref;
}

tree
ipa_struct_relayout::rewrite_offset (tree offset, HOST_WIDE_INT num)
{
  if (TREE_CODE (offset) == INTEGER_CST)
    {
      bool sign = false;
      HOST_WIDE_INT off = TREE_INT_CST_LOW (offset);
      if (off == 0)
	return NULL;
      if (off < 0)
	{
	  off = -off;
	  sign = true;
	}
      if (off % ctype.old_size == 0)
	{
	  HOST_WIDE_INT times = off / ctype.old_size;
	  times = sign ? -times : times;
	  return build_int_cst (TREE_TYPE (offset), num * times);
	}
    }
  return NULL;
}

#define REWRITE_ASSIGN_TREE_IN_STMT(node)		\
do							\
{							\
  tree node = gimple_assign_##node (stmt);		\
  if (node && is_candidate (node))			\
    {							\
      tree mem_ref = rewrite_address (node, gsi);	\
      gimple_assign_set_##node (stmt, mem_ref);		\
      update_stmt (stmt);				\
    }							\
} while (0)

/*       COMPONENT_REF  = exp  =>     MEM_REF = exp
	  /       \		      /     \
       MEM_REF   field		    gptr   offset
       /    \
   pointer offset
*/
bool
ipa_struct_relayout::rewrite_assign (gassign *stmt, gimple_stmt_iterator *gsi)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Maybe rewrite assign:\n");
      print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "to\n");
    }

  switch (gimple_num_ops (stmt))
    {
      case 4: REWRITE_ASSIGN_TREE_IN_STMT (rhs3);  // FALLTHRU
      case 3:
	{
	  REWRITE_ASSIGN_TREE_IN_STMT (rhs2);
	  tree rhs2 = gimple_assign_rhs2 (stmt);
	  if (rhs2 && TREE_CODE (rhs2) == INTEGER_CST)
	    {
	      /* Handle pointer++ and pointer-- or
		 factor is euqal to struct size.  */
	      HOST_WIDE_INT times = 1;
	      if (maybe_rewrite_cst (rhs2, gsi, times))
		{
		  tree tmp = build_int_cst (
				TREE_TYPE (TYPE_SIZE_UNIT (ctype.type)),
				ctype.new_size * times);
		  gimple_assign_set_rhs2 (stmt, tmp);
		  update_stmt (stmt);
		}
	    }
	}  // FALLTHRU
      case 2: REWRITE_ASSIGN_TREE_IN_STMT (rhs1);  // FALLTHRU
      case 1: REWRITE_ASSIGN_TREE_IN_STMT (lhs);   // FALLTHRU
      case 0: break;
      default: gcc_unreachable ();
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "\n");
    }
  return false;
}

bool
ipa_struct_relayout::maybe_rewrite_cst (tree cst, gimple_stmt_iterator *gsi,
					HOST_WIDE_INT &times)
{
  gcc_assert (TREE_CODE (cst) == INTEGER_CST);

  gimple *stmt = gsi_stmt (*gsi);
  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs1 = gimple_assign_rhs1 (stmt);
      if (types_compatible_p (inner_type (TREE_TYPE (rhs1)), ctype.type)
	  || types_compatible_p (inner_type (TREE_TYPE (lhs)), ctype.type))
	{
	  tree num = NULL;
	  if (is_result_of_mult (cst, &num, TYPE_SIZE_UNIT (ctype.type)))
	    {
	      times = TREE_INT_CST_LOW (num);
	      return true;
	    }
	}
    }

  if (gimple_assign_rhs_code (stmt) == MULT_EXPR)
    {
      if (gsi_one_before_end_p (*gsi))
	return false;
      // Check uses.
      imm_use_iterator imm_iter_lhs;
      use_operand_p use_p_lhs;
      FOR_EACH_IMM_USE_FAST (use_p_lhs, imm_iter_lhs, gimple_assign_lhs (stmt))
	{
	  gimple *stmt2 = USE_STMT (use_p_lhs);
	  if (gimple_code (stmt2) != GIMPLE_ASSIGN)
	    continue;
	  if (gimple_assign_rhs_code (stmt2) == POINTER_PLUS_EXPR)
	    {
	      tree lhs = gimple_assign_lhs (stmt2);
	      tree rhs1 = gimple_assign_rhs1 (stmt2);
	      if (types_compatible_p (inner_type (TREE_TYPE (rhs1)), ctype.type)
		  || types_compatible_p (inner_type (TREE_TYPE (lhs)),
					 ctype.type))
		{
		  tree num = NULL;
		  if (is_result_of_mult (cst, &num,
					 TYPE_SIZE_UNIT (ctype.type)))
		    {
		      times = TREE_INT_CST_LOW (num);
		      return true;
		    }
		}
	    }
	  // For pointer compression, handle plus stmt.
	  else if (gimple_assign_rhs_code (stmt2) == PLUS_EXPR)
	    {
	      // Check uses.
	      imm_use_iterator imm_iter_cast;
	      use_operand_p use_p_cast;
	      FOR_EACH_IMM_USE_FAST (use_p_cast, imm_iter_cast,
				     gimple_assign_lhs (stmt2))
		{
		  gimple *stmt_cast = USE_STMT (use_p_cast);
		  if (gimple_code (stmt_cast) != GIMPLE_ASSIGN)
		    continue;
		  if (gimple_assign_cast_p (stmt_cast))
		    {
		      tree lhs_type = inner_type (TREE_TYPE (
					gimple_assign_lhs (stmt_cast)));
		      if (types_compatible_p (lhs_type, ctype.type))
			{
			  tree num = NULL;
			  if (is_result_of_mult (cst, &num,
						 TYPE_SIZE_UNIT (ctype.type)))
			    {
			      times = TREE_INT_CST_LOW (num);
			      return true;
			    }
			}
		    }
		}
	    }
	}
    }
  // For pointer compression, handle div stmt.
  if (gimple_assign_rhs_code (stmt) == TRUNC_DIV_EXPR)
    {
      imm_use_iterator imm_iter;
      use_operand_p use_p;
      tree lhs = gimple_assign_lhs (stmt);
      if (lhs == NULL_TREE)
	return false;
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (is_gimple_debug (use_stmt))
	    continue;
	  if (gimple_code (use_stmt) != GIMPLE_ASSIGN)
	    continue;
	  if (gimple_assign_cast_p (use_stmt))
	    {
	      tree lhs_type = inner_type (TREE_TYPE (
				gimple_assign_lhs (use_stmt)));
	      if (TYPE_UNSIGNED (lhs_type)
		  && TREE_CODE (lhs_type) == INTEGER_TYPE
		  && TYPE_PRECISION (lhs_type) == compressed_size)
		{
		  tree num = NULL;
		  if (is_result_of_mult (cst, &num,
					 TYPE_SIZE_UNIT (ctype.type)))
		    {
		      times = TREE_INT_CST_LOW (num);
		      return true;
		    }
		}
 	    }
 	}
    }
  return false;
}

unsigned int
ipa_struct_relayout::execute (void)
{
  ctype.init_type_info ();
  if (ctype.field_count < min_relayout_split
      || ctype.field_count > max_relayout_split)
    return 0;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Complete Struct Relayout Type: ");
      print_generic_expr (dump_file, ctype.type);
      fprintf (dump_file, "\n");
    }
  transformed++;

  create_global_ptrs ();
  return rewrite ();
}

} // anon namespace


namespace {

/* Methods for ipa_struct_reorg.  */

/* Dump all of the recorded types to file F.  */

void
ipa_struct_reorg::dump_types (FILE *f)
{
  unsigned i;
  srtype *type;
  FOR_EACH_VEC_ELT (types, i, type)
    {
      fprintf (f, "======= the %dth type: ======\n", i);
      type->dump (f);
      fprintf (f, "\n");
    }
}

/* Dump all of the created newtypes to file F.  */

void
ipa_struct_reorg::dump_newtypes (FILE *f)
{
    unsigned i = 0;
    srtype *type = NULL;
    FOR_EACH_VEC_ELT (types, i, type)
    {
	if (!type->has_new_type ())
	  continue;
	fprintf (f, "======= the %dth newtype: ======\n", i);
	fprintf (f, "type : ");
	print_generic_expr (f, type->newtype[0]);
	fprintf (f, "(%d) ", TYPE_UID (type->newtype[0]));
	fprintf (f, "{ ");
	fprintf (f, "\nfields = {\n");

	for (tree field = TYPE_FIELDS (TYPE_MAIN_VARIANT (type->newtype[0]));
				       field; field = DECL_CHAIN (field))
	  {
	    fprintf (f, "field (%d) ", DECL_UID (field));
	    print_generic_expr (f, field);
	    fprintf (f, " {");
	    fprintf (f, "type = ");
	    print_generic_expr (f, TREE_TYPE (field));
	    fprintf (f, "}\n");
	  }
	fprintf (f, "}\n");
	fprintf (f, "size : ");
	print_generic_expr (f, TYPE_SIZE_UNIT (type->newtype[0]));
	fprintf (f, "\n\n");
    }
}

/* Dump all of the recorded types to file F.  */

void
ipa_struct_reorg::dump_types_escaped (FILE *f)
{
  unsigned i;
  srtype *type;
  FOR_EACH_VEC_ELT (types, i, type)
    {
      if (type->has_escaped ())
	{
	  type->simple_dump (f);
	  fprintf (f, " has escaped: \"%s\"\n", type->escape_reason ());
	}
    }
  fprintf (f, "\n");
}

/* Dump all of the record functions to file F.  */

void
ipa_struct_reorg::dump_functions (FILE *f)
{
  unsigned i;
  srfunction *fn;

  fprintf (f, "\n\n");
  globals.dump (f);
  fprintf (f, "\n\n");
  FOR_EACH_VEC_ELT (functions, i, fn)
    {
      fn->dump (f);
      fprintf (f, "\n");
    }
  fprintf (f, "\n\n");
}

/* Find the recorded srtype corresponding to TYPE.  */

srtype *
ipa_struct_reorg::find_type (tree type)
{
  unsigned i;
  /* Get the main variant as we are going
     to find that type only.  */
  type = TYPE_MAIN_VARIANT (type);

  srtype *type1;
  // Search for the type to see if it is already there.
  FOR_EACH_VEC_ELT (types, i, type1)
    {
      if (types_compatible_p (type1->type, type))
	return type1;
    }
  return NULL;
}

/* Is TYPE a volatile type or one which points
   to a volatile type.  */

static bool
isvolatile_type (tree type)
{
  if (TYPE_VOLATILE (type))
    return true;
  while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
    {
      type = TREE_TYPE (type);
      if (TYPE_VOLATILE (type))
	return true;
    }
  return false;
}

/* Is TYPE an array type or points to an array type.  */

static bool
isarraytype (tree type)
{
  if (TREE_CODE (type) == ARRAY_TYPE)
    return true;
  while (POINTER_TYPE_P (type))
    {
      type = TREE_TYPE (type);
      if (TREE_CODE (type) == ARRAY_TYPE)
	return true;
    }
  return false;
}

/*  Is TYPE a pointer to another pointer.  */

static bool
isptrptr (tree type)
{
  if (type == NULL)
    return false;
  bool firstptr = false;
  while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
    {
      if (POINTER_TYPE_P (type))
	{
	  if (firstptr)
	    return true;
	  firstptr = true;
	}
      type = TREE_TYPE (type);
    }
  return false;
}

/* Adding node to map and stack.  */

bool
add_node (tree node, int layers, hash_map <tree, int> &map,
	  auto_vec <tree> &stack)
{
  if (TREE_CODE (node) != SSA_NAME)
    return false;
  if (map.get (node) == NULL)
    {
       if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "	");
	  fprintf (dump_file, "add node: \t\t");
	  print_generic_expr (dump_file, node);
	  fprintf (dump_file, ",\t\tptr layers: %d: \n", layers);
	}
      map.put (node, layers);
      stack.safe_push (node);
    }
  else if (*map.get (node) != layers)
    return false;
  return true;
}

/* Check the number of pointer layers of the gimple phi in definition.  */

bool
check_def_phi (tree def_node, hash_map <tree, int> &ptr_layers)
{
  bool res = true;
  gimple *def_stmt = SSA_NAME_DEF_STMT (def_node);
  for (unsigned j = 0; j < gimple_phi_num_args (def_stmt); j++)
  {
    tree phi_node = gimple_phi_arg_def (def_stmt, j);
    if (integer_zerop (phi_node))
      continue;
    if (ptr_layers.get (phi_node) == NULL)
      return false;
    res &= *ptr_layers.get (def_node) == *ptr_layers.get (phi_node);
  }
  return res;
}

/* Check the number of pointer layers of the gimple assign in definition.  */

bool
check_def_assign (tree def_node, hash_map <tree, int> &ptr_layers)
{
  bool res = true;
  gimple *def_stmt = SSA_NAME_DEF_STMT (def_node);
  gimple_rhs_class rhs_class = gimple_assign_rhs_class (def_stmt);
  tree_code rhs_code = gimple_assign_rhs_code (def_stmt);
  tree rhs1 = gimple_assign_rhs1 (def_stmt);
  tree rhs1_base = TREE_CODE (rhs1) == MEM_REF ? TREE_OPERAND (rhs1, 0) : rhs1;
  if (ptr_layers.get (rhs1_base) == NULL)
    return false;
  if (rhs_class == GIMPLE_SINGLE_RHS || rhs_class == GIMPLE_UNARY_RHS)
    {
      if (TREE_CODE (rhs1) == SSA_NAME)
	res = *ptr_layers.get (def_node) == *ptr_layers.get (rhs1);
      else if (TREE_CODE (rhs1) == MEM_REF)
	res = *ptr_layers.get (def_node)
	      == *ptr_layers.get (TREE_OPERAND (rhs1, 0));
      else
	{
	  return false;
	}
    }
  else if (rhs_class == GIMPLE_BINARY_RHS)
    {
      if (rhs_code == POINTER_PLUS_EXPR)
	res = *ptr_layers.get (def_node) == *ptr_layers.get (rhs1);
      else if (rhs_code == BIT_AND_EXPR)
	res = *ptr_layers.get (def_node) == *ptr_layers.get (rhs1);
      else
	return false;
    }
  else
    return false;
  return res;
}

/* Check node definition.  */

bool
check_node_def (hash_map <tree, int> &ptr_layers)
{
  bool res = true;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n======== check node definition ========\n");
  for (unsigned i = 1; i < num_ssa_names; ++i)
    {
      tree name = ssa_name (i);
      if (name && ptr_layers.get (name) != NULL)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
	  if (dump_file && (dump_flags & TDF_DETAILS)
	      && gimple_code (def_stmt) != GIMPLE_DEBUG)
	    print_gimple_stmt (dump_file, def_stmt, 0);

	  if (gimple_code (def_stmt) == GIMPLE_PHI)
	    res = check_def_phi (name, ptr_layers);
	  else if (gimple_code (def_stmt) == GIMPLE_ASSIGN)
	    res = check_def_assign (name, ptr_layers);
	  else if (gimple_code (def_stmt) == GIMPLE_NOP)
	    continue;
	  else
	    return false;
	}
    }
  return res;
}

/* Check pointer usage.  */

bool
check_record_ptr_usage (gimple *use_stmt, tree &current_node,
			hash_map <tree, int> &ptr_layers,
			auto_vec <tree> &ssa_name_stack)
{
  gimple_rhs_class rhs_class = gimple_assign_rhs_class (use_stmt);
  tree rhs1 = gimple_assign_rhs1 (use_stmt);
  tree lhs = gimple_assign_lhs (use_stmt);
  if (rhs_class != GIMPLE_SINGLE_RHS
      || (TREE_CODE (rhs1) != COMPONENT_REF && TREE_CODE (rhs1) != SSA_NAME)
      || (TREE_CODE (lhs) != MEM_REF && TREE_CODE (lhs) != SSA_NAME))
    return false;

  bool res = true;
  /* MEM[(long int *)a_1] = _1; (record).
     If lhs is ssa_name, lhs cannot be the current node.
     _2 = _1->flow; (No record).  */
  if (TREE_CODE (rhs1) == SSA_NAME)
    {
      tree tmp = (rhs1 != current_node) ? rhs1 : lhs;
      if (TREE_CODE (tmp) == MEM_REF)
	res = add_node (TREE_OPERAND (tmp, 0),
			*ptr_layers.get (current_node) + 1,
			ptr_layers, ssa_name_stack);
      else
	res = add_node (tmp, *ptr_layers.get (current_node),
			ptr_layers, ssa_name_stack);
    }
  else if (TREE_CODE (lhs) == SSA_NAME && TREE_CODE (rhs1) == COMPONENT_REF)
    res = !(POINTER_TYPE_P (TREE_TYPE (rhs1)));
  else
    res = false;
  return res;
}

/* Check and record a single node.  */

bool
check_record_single_node (gimple *use_stmt, tree &current_node,
			  hash_map <tree, int> &ptr_layers,
			  auto_vec <tree> &ssa_name_stack)
{
  gimple_rhs_class rhs_class = gimple_assign_rhs_class (use_stmt);
  tree rhs1 = gimple_assign_rhs1 (use_stmt);
  tree lhs = gimple_assign_lhs (use_stmt);
  gcc_assert (rhs_class == GIMPLE_SINGLE_RHS || rhs_class == GIMPLE_UNARY_RHS);

  if ((TREE_CODE (rhs1) != SSA_NAME && TREE_CODE (rhs1) != MEM_REF)
      || (TREE_CODE (lhs) != SSA_NAME && TREE_CODE (lhs) != MEM_REF))
    return false;

  bool res = true;
  if (TREE_CODE (lhs) == SSA_NAME && TREE_CODE (rhs1) == MEM_REF)
    /* Add such as: _2 = MEM[(struct arc_t * *)_1].  */
    res = add_node (lhs, *ptr_layers.get (current_node) - 1,
		    ptr_layers, ssa_name_stack);
  else if (TREE_CODE (lhs) == MEM_REF && TREE_CODE (rhs1) == SSA_NAME)
    {
      /* Add such as: MEM[(long int *)a_1] = _1.  */
      if (rhs1 == current_node)
	res = add_node (TREE_OPERAND (lhs, 0),
			*ptr_layers.get (current_node) + 1,
			ptr_layers, ssa_name_stack);
      else
	res = add_node (rhs1, *ptr_layers.get (current_node) - 1,
			ptr_layers, ssa_name_stack);
    }
  else if (TREE_CODE (lhs) == SSA_NAME && TREE_CODE (rhs1) == SSA_NAME)
    res = add_node (lhs, *ptr_layers.get (current_node),
		    ptr_layers, ssa_name_stack);
  else
    res = false;

  return res;
}

/* Check and record multiple nodes.  */

bool
check_record_mult_node (gimple *use_stmt, tree &current_node,
			hash_map <tree, int> &ptr_layers,
			auto_vec <tree> &ssa_name_stack)
{
  gimple_rhs_class rhs_class = gimple_assign_rhs_class (use_stmt);
  tree_code rhs_code = gimple_assign_rhs_code (use_stmt);
  tree rhs1 = gimple_assign_rhs1 (use_stmt);
  tree lhs = gimple_assign_lhs (use_stmt);
  tree rhs2 = gimple_assign_rhs2 (use_stmt);
  gcc_assert (rhs_class == GIMPLE_BINARY_RHS);

  if ((rhs_code != POINTER_PLUS_EXPR && rhs_code != POINTER_DIFF_EXPR
       && rhs_code != BIT_AND_EXPR)
      || (TREE_CODE (lhs) != SSA_NAME && TREE_CODE (rhs1) != SSA_NAME))
    return false;

  bool res = true;
  if (rhs_code == POINTER_PLUS_EXPR)
    res = add_node (lhs == current_node ? rhs1 : lhs,
		    *ptr_layers.get (current_node),
		    ptr_layers, ssa_name_stack);
  else if (rhs_code == POINTER_DIFF_EXPR)
    res = add_node (rhs1 != current_node ? rhs1 : rhs2,
		    *ptr_layers.get (current_node),
		    ptr_layers, ssa_name_stack);
  else if (rhs_code == BIT_AND_EXPR)
    {
      if (TREE_CODE (rhs2) != INTEGER_CST)
	return false;
      res = add_node (lhs == current_node ? rhs1 : lhs,
		      *ptr_layers.get (current_node),
		      ptr_layers, ssa_name_stack);
    }
  return res;
}

/* Check whether gimple assign is correctly used and record node.  */

bool
check_record_assign (tree &current_node, gimple *use_stmt,
		     hash_map <tree, int> &ptr_layers,
		     auto_vec <tree> &ssa_name_stack)
{
  gimple_rhs_class rhs_class = gimple_assign_rhs_class (use_stmt);
  if (*ptr_layers.get (current_node) == 1)
    return check_record_ptr_usage (use_stmt, current_node,
				   ptr_layers, ssa_name_stack);
  else if (*ptr_layers.get (current_node) > 1)
    {
      if (rhs_class != GIMPLE_BINARY_RHS
	  && rhs_class != GIMPLE_UNARY_RHS
	  && rhs_class != GIMPLE_SINGLE_RHS)
	return false;

      if (rhs_class == GIMPLE_SINGLE_RHS || rhs_class == GIMPLE_UNARY_RHS)
	return check_record_single_node (use_stmt, current_node,
					 ptr_layers, ssa_name_stack);
      else if (rhs_class == GIMPLE_BINARY_RHS)
	return check_record_mult_node (use_stmt, current_node,
				       ptr_layers, ssa_name_stack);
    }
  else
    return false;

  return true;
}

/* Check whether gimple phi is correctly used and record node.  */

bool
check_record_phi (tree &current_node, gimple *use_stmt,
		  hash_map <tree, int> &ptr_layers,
		  auto_vec <tree> &ssa_name_stack)
{
  bool res = true;
  res &= add_node (gimple_phi_result (use_stmt), *ptr_layers.get (current_node),
		   ptr_layers, ssa_name_stack);

  for (unsigned i = 0; i < gimple_phi_num_args (use_stmt); i++)
    {
      if (integer_zerop (gimple_phi_arg_def (use_stmt, i)))
	continue;
      res &= add_node (gimple_phi_arg_def (use_stmt, i),
		       *ptr_layers.get (current_node),
		       ptr_layers, ssa_name_stack);
    }
  return res;
}

/* Check the use of callee.  */

bool
check_callee (cgraph_node *node, gimple *stmt,
	      hash_map <tree, int> &ptr_layers, int input_layers)
{
  /* caller main ()
	    { spec_qsort.constprop (_649, _651); }
     def    spec_qsort.constprop (void * a, size_t n)
	    { spec_qsort.constprop (a_1, _139); }  */
  /* In safe functions, only call itself is allowed.  */
  if (node->get_edge (stmt)->callee != node)
    return false;
  tree input_node = gimple_call_arg (stmt, 0);
  if (ptr_layers.get (input_node) == NULL
      || *ptr_layers.get (input_node) != input_layers)
    return false;
  if (SSA_NAME_VAR (input_node) != DECL_ARGUMENTS (node->decl))
    return false;

  for (unsigned i = 1; i < gimple_call_num_args (stmt); i++)
    {
      if (ptr_layers.get (gimple_call_arg (stmt, i)) != NULL)
	return false;
    }
  return true;
}

/* Check the usage of input nodes and related nodes.  */

bool
check_node_use (cgraph_node *node, tree current_node,
		hash_map <tree, int> &ptr_layers,
		auto_vec <tree> &ssa_name_stack,
		int input_layers)
{
  imm_use_iterator imm_iter;
  gimple *use_stmt = NULL;
  bool res = true;
  /* Use FOR_EACH_IMM_USE_STMT as an indirect edge
     to search for possible related nodes and push to stack.  */
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, current_node)
    {
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && gimple_code (use_stmt) != GIMPLE_DEBUG)
	{
	  fprintf (dump_file, "%*s", 4, "");
	  print_gimple_stmt (dump_file, use_stmt, 0);
	}
      /* For other types of gimple, do not record the node.  */
      if (res)
	{
	  if (gimple_code (use_stmt) == GIMPLE_PHI)
	    res = check_record_phi (current_node, use_stmt,
				    ptr_layers, ssa_name_stack);
	  else if (gimple_code (use_stmt) == GIMPLE_ASSIGN)
	    res = check_record_assign (current_node, use_stmt,
				       ptr_layers, ssa_name_stack);
	  else if (gimple_code (use_stmt) == GIMPLE_CALL)
	    res = check_callee (node, use_stmt, ptr_layers, input_layers);
	  else if (gimple_code (use_stmt) == GIMPLE_RETURN)
	    res = false;
	}
    }
  return res;
}

/* Trace the pointer layers of void node.  */

bool
get_void_node_ptr_layers (tree input, int &input_layers)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "input type is void* node\n");
  imm_use_iterator imm_iter;
  gimple *use_stmt = NULL;
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, input)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	print_gimple_stmt (dump_file, use_stmt, 0);
      if (gimple_code (use_stmt) == GIMPLE_ASSIGN
	  && gimple_assign_rhs_class (use_stmt) == GIMPLE_SINGLE_RHS)
	{
	  tree rhs1 = gimple_assign_rhs1 (use_stmt);
	  tree lhs = gimple_assign_lhs (use_stmt);
	  if (TREE_CODE (lhs) == SSA_NAME && handled_type (TREE_TYPE (lhs)))
	    {
	      if (TREE_CODE (rhs1) == MEM_REF)
		{
		  input_layers = get_ptr_layers (TREE_TYPE (lhs)) + 1;
		  return true;
		}
	    }
	}
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "end trace pointer layers of void* node\n");
  return false;
}

/* Preparing the First Node for DFS.  */

bool
set_init_node (cgraph_node *node, cgraph_edge *caller,
	       hash_map <tree, int> &ptr_layers,
	       auto_vec <tree> &ssa_name_stack, int &input_layers)
{
  /* Set input_layer
     caller spec_qsort.constprop (_649, _651)
				    |-- Obtains the actual ptr layer
					from the input node.  */
  caller->caller->get_untransformed_body ();
  if (caller->call_stmt == NULL
      || gimple_call_num_args (caller->call_stmt) == 0)
    return false;
  tree input = gimple_call_arg (caller->call_stmt, 0);
  if (!(POINTER_TYPE_P (TREE_TYPE (input))
      || TREE_CODE (TREE_TYPE (input)) == ARRAY_TYPE))
    return false;
  if (handled_type (TREE_TYPE (input)))
    input_layers = get_ptr_layers (TREE_TYPE (input));
  else
    {
      if (VOID_POINTER_P (TREE_TYPE (input)))
	{
	  if (!get_void_node_ptr_layers (input, input_layers))
	    return false;
	}
    }

  /* Set initial node
     def spec_qsort.constprop (void * a, size_t n)
				      |-- Find the initial ssa_name
					  from the parameter node.  */
  tree parm = DECL_ARGUMENTS (node->decl);
  for (unsigned j = 1; j < num_ssa_names; ++j)
    {
      tree name = ssa_name (j);
      if (!name || has_zero_uses (name) || virtual_operand_p (name))
	continue;
      if (SSA_NAME_VAR (name) == parm
	  && gimple_code (SSA_NAME_DEF_STMT (name)) == GIMPLE_NOP)
	{
	  if (!add_node (name, input_layers, ptr_layers, ssa_name_stack))
	    return false;
	}
    }
  return !ssa_name_stack.is_empty ();
}

/* Check the usage of each call.  */

bool
check_each_call (cgraph_node *node, cgraph_edge *caller)
{
  hash_map <tree, int> ptr_layers;
  auto_vec <tree> ssa_name_stack;
  int input_layers = 0;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "======== check each call : %s/%u ========\n",
	     node->name (), node->order);
  if (!set_init_node (node, caller, ptr_layers, ssa_name_stack, input_layers))
    return false;
  int i = 0;
  while (!ssa_name_stack.is_empty ())
    {
      tree current_node = ssa_name_stack.pop ();
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\ncur node %d: \t", i++);
	  print_generic_expr (dump_file, current_node);
	  fprintf (dump_file, ",\t\tptr layers: %d: \n",
		   *ptr_layers.get (current_node));
	}
      if (get_ptr_layers (TREE_TYPE (current_node))
	  > *ptr_layers.get (current_node))
	return false;
      if (!check_node_use (node, current_node, ptr_layers, ssa_name_stack,
			   input_layers))
	return false;
    }

  if (current_fc_level != fc_level::DYNAMIC && !check_node_def (ptr_layers))
    return false;
  return true;
}

/* Filter out function: void func (void*, int n),
   and the function has no static variable, no structure-related variable,
   and no global variable is used.  */

bool
filter_func (cgraph_node *node)
{
  tree parm = DECL_ARGUMENTS (node->decl);
  if (!(parm && VOID_POINTER_P (TREE_TYPE (parm))
	&& VOID_TYPE_P (TREE_TYPE (TREE_TYPE (node->decl)))))
    return false;

  for (parm = DECL_CHAIN (parm); parm; parm = DECL_CHAIN (parm))
    {
      if (TREE_CODE (TREE_TYPE (parm)) != INTEGER_TYPE)
	return false;
    }

  if (DECL_STRUCT_FUNCTION (node->decl)->static_chain_decl)
    return false;

  tree var = NULL_TREE;
  unsigned int i = 0;
  bool res = true;
  FOR_EACH_LOCAL_DECL (cfun, i, var)
    {
      if (TREE_CODE (var) == VAR_DECL && handled_type (TREE_TYPE (var)))
	res = false;
    }
  if (!res)
    return false;

  for (unsigned j = 1; j < num_ssa_names; ++j)
    {
      tree name = ssa_name (j);
      if (!name || has_zero_uses (name) || virtual_operand_p (name))
	continue;
      tree var = SSA_NAME_VAR (name);
      if (var && TREE_CODE (var) == VAR_DECL && is_global_var (var))
	return false;
    }
  return true;
}

/* Check whether the function with the void* parameter and uses the input node
   safely.
   In these functions only component_ref can be used to dereference the last
   layer of the input structure pointer.  The hack operation pointer offset
   after type cast cannot be used.
*/

bool
is_safe_func_with_void_ptr_parm (cgraph_node *node)
{
  if (!filter_func (node))
    return false;

  /* Distinguish Recursive Callers
     normal_callers:    main ()
			{ spec_qsort.constprop (_649, _651); }
     definition:	spec_qsort.constprop (void * a, size_t n)
     recursive_callers: { spec_qsort.constprop (a_1, _139); }  */
  auto_vec<cgraph_edge *> callers = node->collect_callers ();
  auto_vec<cgraph_edge *> normal_callers;
  for (unsigned i = 0; i < callers.length (); i++)
    {
      if (callers[i]->caller != node)
	normal_callers.safe_push (callers[i]);
    }
  if (normal_callers.length () == 0)
    return false;

  for (unsigned i = 0; i < normal_callers.length (); i++)
    {
      if (!check_each_call (node, normal_callers[i]))
	return false;
    }
  return true;
}

/* Return the escape type which corresponds to if
   this is an volatile type, an array type or a pointer
   to a pointer type.  */

static escape_type
escape_type_volatile_array_or_ptrptr (tree type)
{
  if (isvolatile_type (type))
    return escape_volatile;
  if (isarraytype (type))
    return escape_array;
  if (isptrptr (type) && (current_layout_opt_level < STRUCT_REORDER_FIELDS))
    return escape_ptr_ptr;
  return does_not_escape;
}

/* Record field type.  */

void
ipa_struct_reorg::record_field_type (tree field, srtype *base_srtype)
{
  tree field_type = TREE_TYPE (field);
  /* The uid of the type in the structure is different
     from that outside the structure.  */
  srtype *field_srtype = record_type (inner_type (field_type));
  srfield *field_srfield = base_srtype->find_field (int_byte_position (field));
  /* We might have an variable sized type which we don't set the handle.  */
  if (field_srfield)
    {
      field_srfield->type = field_srtype;
      field_srtype->add_field_site (field_srfield);
    }
  if (field_srtype == base_srtype && current_layout_opt_level == STRUCT_SPLIT)
    base_srtype->mark_escape (escape_rescusive_type, NULL);
  /* Types of non-pointer field are difficult to track the correctness
     of the rewrite when it used by the escaped type.  */
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS
      && TREE_CODE (field_type) == RECORD_TYPE)
    field_srtype->mark_escape (escape_instance_field, NULL);
}

/* Record structure all field types.  */

void
ipa_struct_reorg::record_struct_field_types (tree base_type,
					     srtype *base_srtype)
{
  for (tree field = TYPE_FIELDS (base_type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL)
      {
	tree field_type = TREE_TYPE (field);
	process_union (field_type);
	if (TREE_CODE (inner_type (field_type)) == UNION_TYPE
	    || TREE_CODE (inner_type (field_type)) == QUAL_UNION_TYPE)
	  base_srtype->mark_escape (escape_union, NULL);
	if (isvolatile_type (field_type))
	  base_srtype->mark_escape (escape_volatile, NULL);
	escape_type e = escape_type_volatile_array_or_ptrptr (field_type);
	if (e != does_not_escape)
	  base_srtype->mark_escape (e, NULL);
	/* Types of non-pointer field are difficult to track the correctness
	   of the rewrite when it used by the escaped type.  */
	if (current_layout_opt_level >= STRUCT_REORDER_FIELDS
	    && TREE_CODE (field_type) == RECORD_TYPE)
	  base_srtype->mark_escape (escape_instance_field, NULL);
	if (handled_type (field_type))
	  record_field_type (field, base_srtype);
      }
    }
}

/* Record TYPE if not already recorded.  */

srtype *
ipa_struct_reorg::record_type (tree type)
{
  unsigned typeuid;

  /* Get the main variant as we are going
     to record that type only.  */
  type = TYPE_MAIN_VARIANT (type);
  typeuid = TYPE_UID (type);

  srtype *type1;

  type1 = find_type (type);
  if (type1)
    return type1;

  /* If already done recording just return NULL.  */
  if (done_recording)
    return NULL;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Recording new type: %u.\n", typeuid);
      const char *type_name = get_type_name (type);
      if (type_name == NULL)
	fprintf (dump_file, "Recording new type NULL name\n");
      else
	fprintf (dump_file, "Recording new type name: %s.\n", type_name);
    }

  type1 = new srtype (type);
  types.safe_push (type1);

  /* If the type has an user alignment set,
     that means the user most likely already setup the type.  */
  if (TYPE_USER_ALIGN (type))
    type1->mark_escape (escape_user_alignment, NULL);

  record_struct_field_types (type, type1);

  return type1;
}

/* Mark TYPE as escaping with ESCAPES as the reason.  */

void
ipa_struct_reorg::mark_type_as_escape (tree type,
				       escape_type escapes,
				       gimple *stmt)
{
  if (handled_type (type))
    {
      srtype *stype = record_type (inner_type (type));

      if (!stype)
	return;

      stype->mark_escape (escapes, stmt);
    }
}

/* Maybe process the union of type TYPE, such that marking all of the fields'
   types as being escaping.  */

void
ipa_struct_reorg::process_union (tree type)
{
  static hash_set<tree> unions_recorded;

  type = inner_type (type);
  if (TREE_CODE (type) != UNION_TYPE
      && TREE_CODE (type) != QUAL_UNION_TYPE)
    return;

  type = TYPE_MAIN_VARIANT (type);

  /* We already processed this type.  */
  if (unions_recorded.add (type))
    return;

  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL)
	{
	  mark_type_as_escape (TREE_TYPE (field), escape_union);
	  process_union (TREE_TYPE (field));
	}
    }
}

/*  Used by record_var function as a callback to walk_tree.
    Mark the type as escaping if it has expressions which
    cannot be converted for global initializations.  */

static tree
record_init_types (tree *tp, int *walk_subtrees, void *data)
{
  ipa_struct_reorg *c = (ipa_struct_reorg *)data;
  switch (TREE_CODE (*tp))
    {
      CASE_CONVERT:
      case COMPONENT_REF:
      case VIEW_CONVERT_EXPR:
      case ARRAY_REF:
	{
	  tree typeouter = TREE_TYPE (*tp);
	  tree typeinner = TREE_TYPE (TREE_OPERAND (*tp, 0));
	  c->mark_type_as_escape (typeouter, escape_via_global_init);
	  c->mark_type_as_escape (typeinner, escape_via_global_init);
	  break;
	}
      case INTEGER_CST:
	if (!integer_zerop (*tp))
	  c->mark_type_as_escape (TREE_TYPE (*tp), escape_via_global_init);
	break;
     case VAR_DECL:
     case PARM_DECL:
     case FIELD_DECL:
	c->mark_type_as_escape (TREE_TYPE (*tp), escape_via_global_init);
	*walk_subtrees = false;
	break;
     default:
	*walk_subtrees = true;
	break;
    }
  return NULL_TREE;
}

/* Record var DECL; optionally specify the escape reason and the argument
   number in a function.  */

srdecl *
ipa_struct_reorg::record_var (tree decl, escape_type escapes, int arg)
{
  srtype *type;
  srdecl *sd = NULL;

  process_union (TREE_TYPE (decl));

  /* Only the structure type RECORD_TYPE is recorded.
     Therefore, the void* type is filtered out.  */
  if (handled_type (TREE_TYPE (decl)))
    {
      type = record_type (inner_type (TREE_TYPE (decl)));
      escape_type e;

      if (done_recording && !type)
	return NULL;

      gcc_assert (type);
      if (TREE_CODE (decl) == VAR_DECL && is_global_var (decl))
	sd = globals.record_decl (type, decl, arg);
      else
	{
	  gcc_assert (current_function);
	  sd = current_function->record_decl (type, decl, arg);
	}

      /* If the variable has the "used" attribute,
	 then treat the type as escaping.  */
      if (escapes != does_not_escape)
	e = escapes;
      else if (TREE_CODE (decl) != SSA_NAME && DECL_PRESERVE_P (decl))
	e = escape_marked_as_used;
      else if (TREE_THIS_VOLATILE (decl))
	e = escape_volatile;
      else if (TREE_CODE (decl) != SSA_NAME && DECL_USER_ALIGN (decl))
	e = escape_user_alignment;
      else if (TREE_CODE (decl) != SSA_NAME && TREE_STATIC (decl)
	       && TREE_PUBLIC (decl))
	e = escape_via_global_var;
      /* We don't have an initlizer.  */
      else if (TREE_CODE (decl) != SSA_NAME
	       && DECL_INITIAL (decl) == error_mark_node)
	e = escape_via_global_var;
      else
	e = escape_type_volatile_array_or_ptrptr (TREE_TYPE (decl));

      /* Separate instance is hard to trace in complete struct
	 relayout optimization.  */
      if (current_layout_opt_level >= COMPLETE_STRUCT_RELAYOUT
	  && TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE)
	e = escape_separate_instance;

      if (e != does_not_escape
	  && (current_layout_opt_level != COMPLETE_STRUCT_RELAYOUT
	      || replace_type_map.get (type->type) == NULL))
	type->mark_escape (e, NULL);
    }

  /* Record the initial usage of variables as types escapes.  */
  if (TREE_CODE (decl) != SSA_NAME && TREE_STATIC (decl)
      && DECL_INITIAL (decl))
    {
      walk_tree_without_duplicates (&DECL_INITIAL (decl),
				    record_init_types, this);
      if (!integer_zerop (DECL_INITIAL (decl))
	  && DECL_INITIAL (decl) != error_mark_node)
	mark_type_as_escape (TREE_TYPE (decl), escape_via_global_init);
    }
  return sd;
}

/* Find void* ssa_names which are used inside MEM[] or if we have &a.c,
   mark the type as escaping.  */

void
ipa_struct_reorg::find_var (tree expr, gimple *stmt)
{
  /* If we have VCE<a> mark the outer type as escaping and the inner one
     Also mark the inner most operand.  */
  if (TREE_CODE (expr) == VIEW_CONVERT_EXPR)
    {
      mark_type_as_escape (TREE_TYPE (expr), escape_vce, stmt);
      mark_type_as_escape (TREE_TYPE (TREE_OPERAND (expr, 0)),
			   escape_vce, stmt);
    }

  /* If we have &b.c then we need to mark the type of b
     as escaping as tracking a will be hard.  */
  if (TREE_CODE (expr) == ADDR_EXPR
      || TREE_CODE (expr) == VIEW_CONVERT_EXPR)
    {
      tree r = TREE_OPERAND (expr, 0);
      tree orig_type = TREE_TYPE (expr);
      if (handled_component_p (r) || TREE_CODE (r) == MEM_REF)
	{
	  while (handled_component_p (r) || TREE_CODE (r) == MEM_REF)
	    {
	      if (TREE_CODE (r) == VIEW_CONVERT_EXPR)
		{
		  mark_type_as_escape (TREE_TYPE (r), escape_vce, stmt);
		  mark_type_as_escape (TREE_TYPE (TREE_OPERAND (r, 0)),
				       escape_vce, stmt);
		}
	      if (TREE_CODE (r) == MEM_REF)
		{
		  mark_type_as_escape (TREE_TYPE (TREE_OPERAND (r, 1)),
				       escape_addr, stmt);
		  tree inner_type = TREE_TYPE (TREE_OPERAND (r, 0));
		  if (orig_type != inner_type)
		    {
		      mark_type_as_escape (orig_type,
					   escape_cast_another_ptr, stmt);
		      mark_type_as_escape (inner_type,
					   escape_cast_another_ptr, stmt);
		    }
		}
	      r = TREE_OPERAND (r, 0);
	    }
	  mark_expr_escape (r, escape_addr, stmt);
	}
    }

  tree base;
  bool indirect;
  srtype *type;
  srfield *field;
  bool realpart, imagpart, address;
  bool escape_from_base = false;
  /* The should_create flag is true, the declaration can be recorded.  */
  get_type_field (expr, base, indirect, type, field,
		  realpart, imagpart, address, escape_from_base, true, true);
}

void
ipa_struct_reorg::find_vars (gimple *stmt)
{
  gasm *astmt;
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      if (gimple_assign_rhs_class (stmt) == GIMPLE_SINGLE_RHS
	  || gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
	  || gimple_assign_rhs_code (stmt) == NOP_EXPR)
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  tree rhs = gimple_assign_rhs1 (stmt);
	  find_var (gimple_assign_lhs (stmt), stmt);
	  /* _2 = MEM[(struct arc_t * *)_1];
	     records the right value _1 declaration.  */
	  find_var (gimple_assign_rhs1 (stmt), stmt);

	  /* Pointer types from non-zero pointer need to be escaped in pointer
	     compression and complete relayout.
	     e.g _1->t = (struct *) 0x400000.  */
	  if (current_layout_opt_level >= COMPLETE_STRUCT_RELAYOUT
	      && TREE_CODE (lhs) == COMPONENT_REF
	      && TREE_CODE (TREE_TYPE (lhs)) == POINTER_TYPE
	      && TREE_CODE (rhs) == INTEGER_CST
	      && !integer_zerop (rhs))
	    {
	      mark_type_as_escape (inner_type (TREE_TYPE (lhs)),
				   escape_cast_int, stmt);
	    }

	  /* Add a safe func mechanism.  */
	  bool l_find = true;
	  bool r_find = true;
	  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	    {
	      l_find = !(current_function->is_safe_func
			 && TREE_CODE (lhs) == SSA_NAME
			 && is_from_void_ptr_parm (lhs));
	      r_find = !(current_function->is_safe_func
			 && TREE_CODE (rhs) == SSA_NAME
			 && is_from_void_ptr_parm (rhs));
	    }

	  if ((TREE_CODE (lhs) == SSA_NAME)
	      && VOID_POINTER_P (TREE_TYPE (lhs))
	      && handled_type (TREE_TYPE (rhs)) && l_find)
	    {
	      srtype *t = find_type (inner_type (TREE_TYPE (rhs)));
	      srdecl *d = find_decl (lhs);
	      if (!d && t)
		{
		  current_function->record_decl (t, lhs, -1,
			isptrptr (TREE_TYPE (rhs)) ? TREE_TYPE (rhs) : NULL);
		  tree var = SSA_NAME_VAR (lhs);
		  if (var && VOID_POINTER_P (TREE_TYPE (var)))
		    current_function->record_decl (t, var, -1,
			isptrptr (TREE_TYPE (rhs)) ? TREE_TYPE (rhs) : NULL);
		}
	    }
	  /* Find void ssa_name such as:
	     void * _1; struct arc * _2;
	     _2 = _1 + _3; _1 = calloc (100, 40).  */
	  if (TREE_CODE (rhs) == SSA_NAME
	      && VOID_POINTER_P (TREE_TYPE (rhs))
	      && handled_type (TREE_TYPE (lhs)) && r_find)
	    {
	      srtype *t = find_type (inner_type (TREE_TYPE (lhs)));
	      srdecl *d = find_decl (rhs);
	      if (!d && t)
		{
		  current_function->record_decl (t, rhs, -1,
			isptrptr (TREE_TYPE (lhs)) ? TREE_TYPE (lhs) : NULL);
		  tree var = SSA_NAME_VAR (rhs);
		  if (var && VOID_POINTER_P (TREE_TYPE (var)))
		    current_function->record_decl (t, var, -1,
			isptrptr (TREE_TYPE (lhs)) ? TREE_TYPE (lhs) : NULL);
		}
	    }
	}
      else if ((current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	       && (gimple_assign_rhs_code (stmt) == LE_EXPR
		   || gimple_assign_rhs_code (stmt) == LT_EXPR
		   || gimple_assign_rhs_code (stmt) == GE_EXPR
		   || gimple_assign_rhs_code (stmt) == GT_EXPR))
	{
	  find_var (gimple_assign_lhs (stmt), stmt);
	  find_var (gimple_assign_rhs1 (stmt), stmt);
	  find_var (gimple_assign_rhs2 (stmt), stmt);
	}
      /* Find void ssa_name from stmt such as: _2 = _1 - old_arcs_1.  */
      else if ((current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	       && gimple_assign_rhs_code (stmt) == POINTER_DIFF_EXPR
	       && types_compatible_p (
		  TYPE_MAIN_VARIANT (TREE_TYPE (gimple_assign_rhs1 (stmt))),
		  TYPE_MAIN_VARIANT (TREE_TYPE (gimple_assign_rhs2 (stmt)))))
	{
	  find_var (gimple_assign_rhs1 (stmt), stmt);
	  find_var (gimple_assign_rhs2 (stmt), stmt);
	}
      else if (gimple_assign_rhs_code (stmt) == EQ_EXPR
	       && types_compatible_p (
		  TYPE_MAIN_VARIANT (TREE_TYPE (gimple_assign_rhs1 (stmt))),
		  TYPE_MAIN_VARIANT (TREE_TYPE (gimple_assign_rhs2 (stmt)))))
	{
	  find_var (gimple_assign_rhs1 (stmt), stmt);
	  find_var (gimple_assign_rhs2 (stmt), stmt);
	}
      else
	{
	  /* Because we won't handle these stmts in rewrite phase,
	     just mark these types as escaped.  */
	  switch (gimple_num_ops (stmt))
	    {
	      case 4: mark_type_as_escape (
			TREE_TYPE (gimple_assign_rhs3 (stmt)),
			escape_unhandled_rewrite, stmt);
			// FALLTHRU
	      case 3: mark_type_as_escape (
			TREE_TYPE (gimple_assign_rhs2 (stmt)),
			escape_unhandled_rewrite, stmt);
			// FALLTHRU
	      case 2: mark_type_as_escape (
			TREE_TYPE (gimple_assign_rhs1 (stmt)),
			escape_unhandled_rewrite, stmt);
			// FALLTHRU
	      case 1: mark_type_as_escape (
			TREE_TYPE (gimple_assign_lhs (stmt)),
			escape_unhandled_rewrite, stmt);
			// FALLTHRU
	      case 0: break;
	      default: gcc_unreachable ();
	    }
	}
      break;

    case GIMPLE_CALL:
      if (gimple_call_lhs (stmt))
	find_var (gimple_call_lhs (stmt), stmt);

      if (gimple_call_chain (stmt))
	find_var (gimple_call_chain (stmt), stmt);

      for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
	find_var (gimple_call_arg (stmt, i), stmt);
      break;

    case GIMPLE_ASM:
      astmt = as_a <gasm *> (stmt);
      for (unsigned i = 0; i < gimple_asm_ninputs (astmt); i++)
	find_var (TREE_VALUE (gimple_asm_input_op (astmt, i)), stmt);
      for (unsigned i = 0; i < gimple_asm_noutputs (astmt); i++)
	find_var (TREE_VALUE (gimple_asm_output_op (astmt, i)), stmt);
      mark_types_asm (astmt);
      break;

    case GIMPLE_RETURN:
      {
	tree expr = gimple_return_retval (as_a <greturn *> (stmt));
	if (expr)
	  find_var (expr, stmt);
	/* return &a; should mark the type of a as escaping
	   through a return.  */
	if (expr && TREE_CODE (expr) == ADDR_EXPR)
	  {
	    expr = TREE_OPERAND (expr, 0);
	    srdecl *d = find_decl (expr);
	    if (d)
	      d->type->mark_escape (escape_return, stmt);
	  }
      }
      break;

    default:
      break;
    }
}

static HOST_WIDE_INT
get_offset (tree op, HOST_WIDE_INT offset)
{
  switch (TREE_CODE (op))
    {
      case COMPONENT_REF:
	{
	  return int_byte_position (TREE_OPERAND (op, 1));
	}
      case MEM_REF:
	{
	  return tree_to_uhwi (TREE_OPERAND (op, 1));
	}
      default:
	return offset;
    }
  return offset;
}

/* Record field access.  */
static void
record_field_access (tree type, HOST_WIDE_INT offset,
		     unsigned access, void *data)
{
  srtype *this_srtype = ((ipa_struct_reorg *)data)->find_type (type);
  if (this_srtype == NULL)
    return;
  srfield *this_srfield = this_srtype->find_field (offset);
  if (this_srfield == NULL)
    return;

  this_srfield->field_access |= access;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "record field access %d:", access);
      print_generic_expr (dump_file, type);
      fprintf (dump_file, "  field:");
      print_generic_expr (dump_file, this_srfield->fielddecl);
      fprintf (dump_file, "\n");
    }
  return;

}

/* Update field_access in srfield.  */

static void
update_field_access (tree node, tree op, unsigned access, void *data)
{
  HOST_WIDE_INT offset = 0;
  offset = get_offset (op, offset);
  tree node_type = inner_type (TREE_TYPE (node));
  record_field_access (node_type, offset, access, data);
  tree base = node;
  get_base (base, node);
  tree base_type = inner_type (TREE_TYPE (base));
  if (!types_compatible_p (base_type, node_type))
    {
      record_field_access (base_type, get_offset (node, offset),
			   access, data);
    }
  return;
}

/* A callback for walk_stmt_load_store_ops to visit store.  */

static bool
find_field_p_store (gimple *stmt ATTRIBUTE_UNUSED,
		    tree node, tree op, void *data)
{
  update_field_access (node, op, WRITE_FIELD, data);

  return false;
}

/* A callback for walk_stmt_load_store_ops to visit load.  */

static bool
find_field_p_load (gimple *stmt ATTRIBUTE_UNUSED,
		   tree node, tree op, void *data)
{
  update_field_access (node, op, READ_FIELD, data);

  return false;
}

/* Determine whether the stmt should be deleted.  */

bool
ipa_struct_reorg::remove_dead_field_stmt (tree lhs)
{
  tree base = NULL_TREE;
  bool indirect = false;
  srtype *t = NULL;
  srfield *f = NULL;
  bool realpart = false;
  bool imagpart = false;
  bool address = false;
  bool escape_from_base = false;
  if (!get_type_field (lhs, base, indirect, t, f, realpart, imagpart,
		       address, escape_from_base))
    return false;
  if (t ==NULL)
    return false;
  if (t->newtype[0] == t->type)
    return false;
  if (f == NULL)
    return false;
  if (f->newfield[0] == NULL)
    return true;
  return false;
}

/* Maybe record access of statement for further analaysis.  */

void
ipa_struct_reorg::maybe_record_stmt (cgraph_node *node, gimple *stmt)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      maybe_record_assign (node, as_a <gassign *> (stmt));
      break;
    case GIMPLE_CALL:
      maybe_record_call (node, as_a <gcall *> (stmt));
      break;
    case GIMPLE_DEBUG:
      break;
    case GIMPLE_GOTO:
    case GIMPLE_SWITCH:
      break;
    default:
      break;
    }
  if (current_layout_opt_level & DEAD_FIELD_ELIMINATION)
    {
      /* Look for loads and stores.  */
      walk_stmt_load_store_ops (stmt, this, find_field_p_load,
				find_field_p_store);
    }
}

/* Calculate the multiplier.  */

static bool
calculate_mult_num (tree arg, tree *num, tree struct_size)
{
  gcc_assert (TREE_CODE (arg) == INTEGER_CST);
  bool sign = false;
  HOST_WIDE_INT size = TREE_INT_CST_LOW (arg);
  if (size < 0)
    {
      size = -size;
      sign = true;
    }
  tree arg2 = build_int_cst (TREE_TYPE (arg), size);
  if (integer_zerop (size_binop (FLOOR_MOD_EXPR, arg2, struct_size)))
    {
      tree number = size_binop (FLOOR_DIV_EXPR, arg2, struct_size);
      if (sign)
	number = build_int_cst (TREE_TYPE (number), -tree_to_shwi (number));
      *num = number;
      return true;
    }
  return false;
}

/* Trace and calculate the multiplier of PLUS_EXPR.  */

static bool
trace_calculate_plus (gimple *size_def_stmt, tree *num, tree struct_size)
{
  gcc_assert (gimple_assign_rhs_code (size_def_stmt) == PLUS_EXPR);

  tree num1 = NULL_TREE;
  tree num2 = NULL_TREE;
  tree arg0 = gimple_assign_rhs1 (size_def_stmt);
  tree arg1 = gimple_assign_rhs2 (size_def_stmt);
  if (!is_result_of_mult (arg0, &num1, struct_size) || num1 == NULL_TREE)
    return false;
  if (!is_result_of_mult (arg1, &num2, struct_size) || num2 == NULL_TREE)
    return false;
  *num = size_binop (PLUS_EXPR, num1, num2);
  return true;
}

/* Trace and calculate the multiplier of MULT_EXPR.  */

static bool
trace_calculate_mult (gimple *size_def_stmt, tree *num, tree struct_size)
{
  gcc_assert (gimple_assign_rhs_code (size_def_stmt) == MULT_EXPR);

  tree arg0 = gimple_assign_rhs1 (size_def_stmt);
  tree arg1 = gimple_assign_rhs2 (size_def_stmt);
  tree num1 = NULL_TREE;

  if (is_result_of_mult (arg0, &num1, struct_size) && num1 != NULL_TREE)
    {
      *num = size_binop (MULT_EXPR, arg1, num1);
      return true;
    }
  if (is_result_of_mult (arg1, &num1, struct_size) && num1 != NULL_TREE)
    {
      *num = size_binop (MULT_EXPR, arg0, num1);
      return true;
    }
  *num = NULL_TREE;
  return false;
}

/* Trace and calculate the multiplier of NEGATE_EXPR.  */

static bool
trace_calculate_negate (gimple *size_def_stmt, tree *num, tree struct_size)
{
  gcc_assert (gimple_assign_rhs_code (size_def_stmt) == NEGATE_EXPR);

  /* Support NEGATE_EXPR trace: _3 = -_2; _2 = _1 * 72.  */
  tree num1 = NULL_TREE;
  tree arg0 = gimple_assign_rhs1 (size_def_stmt);
  if (!is_result_of_mult (arg0, &num1, struct_size) || num1 == NULL_TREE)
    return false;
  tree num0 = build_int_cst (TREE_TYPE (num1), -1);
  *num = size_binop (MULT_EXPR, num0, num1);
  return true;
}

/* Trace and calculate the multiplier of POINTER_DIFF_EXPR.  */

static bool
trace_calculate_diff (gimple *size_def_stmt, tree *num)
{
  gcc_assert (gimple_assign_rhs_code (size_def_stmt) == NOP_EXPR);

  /* Support POINTER_DIFF_EXPR trace:
  _3 = (long unsigned int) _2; _2 = _1 - old_arcs_1.  */
  tree arg = gimple_assign_rhs1 (size_def_stmt);
  size_def_stmt = SSA_NAME_DEF_STMT (arg);
  if (size_def_stmt && is_gimple_assign (size_def_stmt)
      && gimple_assign_rhs_code (size_def_stmt) == POINTER_DIFF_EXPR)
    {
      *num = NULL_TREE;
      return true;
    }
  *num = NULL_TREE;
  return false;
}

/* This function checks whether ARG is a result of multiplication
   of some number by STRUCT_SIZE.  If yes, the function returns true
   and this number is filled into NUM.  */

static bool
is_result_of_mult (tree arg, tree *num, tree struct_size)
{
  if (!struct_size
      || TREE_CODE (struct_size) != INTEGER_CST
      || integer_zerop (struct_size))
    return false;

  /* If we have a integer, just check if it is a multiply of STRUCT_SIZE.  */
  if (TREE_CODE (arg) == INTEGER_CST)
    return calculate_mult_num (arg, num, struct_size);

  gimple *size_def_stmt = SSA_NAME_DEF_STMT (arg);

  /* If the allocation statement was of the form
     D.2229_10 = <alloc_func> (D.2228_9);
     then size_def_stmt can be D.2228_9 = num.3_8 * 8;  */

  while (size_def_stmt && is_gimple_assign (size_def_stmt))
    {
      tree lhs = gimple_assign_lhs (size_def_stmt);

      /* We expect temporary here.  */
      if (!is_gimple_reg (lhs))
	return false;

      // FIXME: this should handle SHIFT also.
      tree_code rhs_code = gimple_assign_rhs_code (size_def_stmt);
      if (rhs_code == PLUS_EXPR)
	return trace_calculate_plus (size_def_stmt, num, struct_size);
      else if (rhs_code == MULT_EXPR)
	return trace_calculate_mult (size_def_stmt, num, struct_size);
      else if (rhs_code == SSA_NAME)
	{
	  arg = gimple_assign_rhs1 (size_def_stmt);
	  size_def_stmt = SSA_NAME_DEF_STMT (arg);
	}
      else if (rhs_code == NEGATE_EXPR
	       && current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	return trace_calculate_negate (size_def_stmt, num, struct_size);
      else if (rhs_code == NOP_EXPR
	       && current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	return trace_calculate_diff (size_def_stmt, num);
      else
	{
	  *num = NULL_TREE;
	  return false;
	}
    }

  *num = NULL_TREE;
  return false;
}

/* Return TRUE if STMT is an allocation statement that is handled.  */

bool
ipa_struct_reorg::handled_allocation_stmt (gimple *stmt)
{
  if ((current_layout_opt_level & STRUCT_REORDER_FIELDS)
      && (gimple_call_builtin_p (stmt, BUILT_IN_REALLOC)
	  || gimple_call_builtin_p (stmt, BUILT_IN_MALLOC)
	  || gimple_call_builtin_p (stmt, BUILT_IN_CALLOC)))
    return true;
  if ((current_layout_opt_level == COMPLETE_STRUCT_RELAYOUT
       || current_layout_opt_level & POINTER_COMPRESSION_SAFE)
      && gimple_call_builtin_p (stmt, BUILT_IN_CALLOC))
    return true;
  if ((current_layout_opt_level == STRUCT_SPLIT)
      && (gimple_call_builtin_p (stmt, BUILT_IN_REALLOC)
	  || gimple_call_builtin_p (stmt, BUILT_IN_MALLOC)
	  || gimple_call_builtin_p (stmt, BUILT_IN_CALLOC)
	  || gimple_call_builtin_p (stmt, BUILT_IN_ALIGNED_ALLOC)
	  || gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA)
	  || gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA_WITH_ALIGN)))
    return true;
  return false;
}

/* Returns the allocated size / T size for STMT.  That is the number of
   elements in the array allocated.  */

tree
ipa_struct_reorg::allocate_size (srtype *type, srdecl *decl, gimple *stmt)
{
  if (type->has_escaped ())
    return NULL;

  return get_allocate_size (type->type, decl->decl, decl->orig_type, stmt);
}

void
ipa_struct_reorg::maybe_mark_or_record_other_side (tree side, tree other,
						   gimple *stmt)
{
  gcc_assert (TREE_CODE (side) == SSA_NAME || TREE_CODE (side) == ADDR_EXPR);
  srtype *type = NULL;
  if (handled_type (TREE_TYPE (other)))
    type = record_type (inner_type (TREE_TYPE (other)));
  if (TREE_CODE (side) == ADDR_EXPR)
    side = TREE_OPERAND (side, 0);
  srdecl *d = find_decl (side);
  if (!type)
    {
      if (!d)
	return;
      if (TREE_CODE (side) == SSA_NAME
	  && VOID_POINTER_P (TREE_TYPE (side)))
	return;
      if (current_layout_opt_level != COMPLETE_STRUCT_RELAYOUT
	  || replace_type_map.get (d->type->type) == NULL)
	d->type->mark_escape (escape_cast_another_ptr, stmt);
      return;
    }

  if (!d)
    {
      /* MEM[(struct arc *)_1].head = _2; _2 = calloc (100, 104).  */
      if (VOID_POINTER_P (TREE_TYPE (side))
	  && TREE_CODE (side) == SSA_NAME)
	{
	  tree inner = SSA_NAME_VAR (side);
	  if (inner)
	    {
	      srdecl *in = find_decl (inner);
	      if (in && !in->type->has_escaped ())
		{
		  /* The type is other, the declaration is side.  */
		  current_function->record_decl (type, side, -1,
			isptrptr (TREE_TYPE (other)) ? TREE_TYPE (other) : NULL);
		}
	     }
	}
      else
	/* *_1 = &MEM[(void *)&x + 8B].  */
	if (current_layout_opt_level != COMPLETE_STRUCT_RELAYOUT
	    || replace_type_map.get (type->type) == NULL)
	    type->mark_escape (escape_cast_another_ptr, stmt);
    }
  else if (type != d->type)
    {
      if (!is_replace_type (d->type->type, type->type))
	{
	  type->mark_escape (escape_cast_another_ptr, stmt);
	  d->type->mark_escape (escape_cast_another_ptr, stmt);
	}
    }
  /* x_1 = y.x_nodes; void *x;
     Mark the structure pointer type assigned
     to the void* variable as escape.  Unless the void* is only used to compare
     with variables of the same type.  */
  else if (current_layout_opt_level >= STRUCT_REORDER_FIELDS
	   && TREE_CODE (side) == SSA_NAME
	   && VOID_POINTER_P (TREE_TYPE (side))
	   && SSA_NAME_VAR (side)
	   && VOID_POINTER_P (TREE_TYPE (SSA_NAME_VAR (side))))
      if (current_layout_opt_level < POINTER_COMPRESSION_SAFE
	  || !safe_void_cmp_p (side, type))
	{
	  mark_type_as_escape (TREE_TYPE (other), escape_cast_void, stmt);
	}

  check_ptr_layers (side, other, stmt);
}

/* Record accesses in an assignment statement STMT.  */

void
ipa_struct_reorg::maybe_record_assign (cgraph_node *node, gassign *stmt)
{
  if (gimple_clobber_p (stmt))
    {
      record_stmt_expr (gimple_assign_lhs (stmt), node, stmt);
      return;
    }

  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree num;
      if (!handled_type (TREE_TYPE (lhs)))
	return;
      /* Check if rhs2 is a multiplication of the size of the type.  */
      /* The size adjustment and judgment of multi-layer pointers
	 are added.  */
      if (is_result_of_mult (rhs2, &num,
			     isptrptr (TREE_TYPE (lhs))
			     ? TYPE_SIZE_UNIT (TREE_TYPE (lhs))
			     : TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (lhs)))))
	{
	  record_stmt_expr (lhs, node, stmt);
	  record_stmt_expr (rhs1, node, stmt);
	}
      else
	{
	  mark_expr_escape (lhs, escape_non_multiply_size, stmt);
	  mark_expr_escape (rhs1, escape_non_multiply_size, stmt);
	}
      return;
    }
  /* Copies, References, Taking addresses.  */
  if (gimple_assign_rhs_class (stmt) == GIMPLE_SINGLE_RHS)
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs = gimple_assign_rhs1 (stmt);
      /* If we have a = &b.c then we need to mark the type of b
	 as escaping as tracking a will be hard.  */
      if (TREE_CODE (rhs) == ADDR_EXPR)
	{
	  tree r = TREE_OPERAND (rhs, 0);
	  if (handled_component_p (r))
	    {
	      while (handled_component_p (r))
		r = TREE_OPERAND (r, 0);
	      mark_expr_escape (r, escape_addr, stmt);
	      return;
	    }
	}
      if ((TREE_CODE (rhs) == SSA_NAME || TREE_CODE (rhs) == ADDR_EXPR))
	maybe_mark_or_record_other_side (rhs, lhs, stmt);
      if (TREE_CODE (lhs) == SSA_NAME)
	maybe_mark_or_record_other_side (lhs, rhs, stmt);

      /* Handle missing ARRAY_REF cases.  */
      if (TREE_CODE (lhs) == ARRAY_REF)
	mark_type_as_escape (TREE_TYPE (lhs), escape_array, stmt);
      if (TREE_CODE (rhs) == ARRAY_REF)
	mark_type_as_escape (TREE_TYPE (rhs), escape_array, stmt);

      record_stmt_expr (lhs, node, stmt);
      record_stmt_expr (rhs, node, stmt);
    }
}

static bool
check_mem_ref_offset (tree expr, tree *num)
{
  bool ret = false;

  if (TREE_CODE (expr) != MEM_REF)
    return false;

  /* Try to find the structure size.  */
  tree field_off = TREE_OPERAND (expr, 1);
  tree tmp = TREE_OPERAND (expr, 0);
  if (TREE_CODE (tmp) == ADDR_EXPR)
    tmp = TREE_OPERAND (tmp, 0);
  /* Specify the correct size for the multi-layer pointer.  */
  tree size = isptrptr (TREE_TYPE (tmp))
			? TYPE_SIZE_UNIT (TREE_TYPE (tmp))
			: TYPE_SIZE_UNIT (inner_type (TREE_TYPE (tmp)));
  ret = is_result_of_mult (field_off, num, size);
  return ret;
}

static tree
get_ref_base_and_offset (tree &e, HOST_WIDE_INT &offset,
			 bool &realpart, bool &imagpart,
			 tree &accesstype, tree *num)
{
  offset = 0;
  realpart = false;
  imagpart = false;
  accesstype = NULL_TREE;
  if (TREE_CODE (e) == REALPART_EXPR)
    {
      e = TREE_OPERAND (e, 0);
      realpart = true;
    }
  if (TREE_CODE (e) == IMAGPART_EXPR)
    {
      e = TREE_OPERAND (e, 0);
      imagpart = true;
    }
  tree expr = e;
  while (true)
    {
      switch (TREE_CODE (expr))
	{
	  case COMPONENT_REF:
	  {
	    /* x.a = _1; If expr is the lvalue of stmt,
	       then field type is FIELD_DECL - POINTER_TYPE - RECORD_TYPE.  */
	    tree field = TREE_OPERAND (expr, 1);
	    tree field_off = byte_position (field);
	    if (TREE_CODE (field_off) != INTEGER_CST)
	      return NULL;
	    offset += tree_to_shwi (field_off);
	    /* x.a = _1; If expr is the lvalue of stmt,
	       then expr type is VAR_DECL - RECORD_TYPE (fetch x) */
	    expr = TREE_OPERAND (expr, 0);
	    accesstype = NULL;
	    break;
	  }
	  case MEM_REF:
	  {
	    /* _2 = MEM[(struct s * *)_1];
	       If expr is the right value of stmt, then field_off type is
	       INTEGER_CST - POINTER_TYPE - POINTER_TYPE - RECORD_TYPE.  */
	    tree field_off = TREE_OPERAND (expr, 1);
	    gcc_assert (TREE_CODE (field_off) == INTEGER_CST);
	    /* So we can mark the types as escaping if different.  */
	    accesstype = TREE_TYPE (field_off);
	    if (!check_mem_ref_offset (expr, num))
	      offset += tree_to_uhwi (field_off);
	    return TREE_OPERAND (expr, 0);
	  }
	  default:
	    return expr;
	}
    }
}

/* Return true if EXPR was accessing the whole type T.  */

bool
ipa_struct_reorg::wholeaccess (tree expr, tree base,
			       tree accesstype, srtype *t)
{
  if (expr == base)
    return true;

  if (TREE_CODE (expr) == ADDR_EXPR && TREE_OPERAND (expr, 0) == base)
    return true;

  if (!accesstype || !handled_type (TREE_TYPE (expr)) || !t || !t->type)
    return false;

  /* T *_1; _2 = MEM[(T *)_1].  */
  if (TREE_CODE (expr) == MEM_REF
      && integer_zerop (TREE_OPERAND (expr, 1))
      && POINTER_TYPE_P (TREE_TYPE (base))
      && types_compatible_p (TREE_TYPE (TREE_TYPE (base)), t->type))
    return POINTER_TYPE_P (accesstype)
	   && types_compatible_p (TREE_TYPE (accesstype), t->type);

  if (!types_compatible_p (TREE_TYPE (expr), TREE_TYPE (accesstype)))
    return false;

  return t == find_type (inner_type (TREE_TYPE (expr)));
}

bool
ipa_struct_reorg::get_type_field (tree expr, tree &base, bool &indirect,
				  srtype *&type, srfield *&field,
				  bool &realpart, bool &imagpart, bool &address,
				  bool &escape_from_base, bool should_create,
				  bool can_escape)
{
  tree num = NULL_TREE;
  HOST_WIDE_INT offset;
  tree accesstype;
  address = false;
  bool mark_as_bit_field = false;

  if (TREE_CODE (expr) == BIT_FIELD_REF)
    {
      expr = TREE_OPERAND (expr, 0);
      mark_as_bit_field = true;
    }

  /* Ref is classified into two types: COMPONENT_REF or MER_REF.  */
  base = get_ref_base_and_offset (expr, offset, realpart, imagpart,
				  accesstype, &num);

  /* Variable access, unkown type.  */
  if (base == NULL)
    return false;

  if (TREE_CODE (base) == ADDR_EXPR)
    {
      address = true;
      base = TREE_OPERAND (base, 0);
    }

  srdecl *d = find_decl (base);
  srtype *t;

  if (integer_zerop (base))
    {
      gcc_assert (!d);
      if (!accesstype)
	return false;
      t = find_type (inner_type (inner_type (accesstype)));
      if (!t && should_create && handled_type (accesstype))
	t = record_type (inner_type (accesstype));
      if (!t)
	return false;
    }
  /* If no such decl is finded
     or orig_type is not added to this decl, then add it.  */
  else if (!d && accesstype)
    {
      if (!should_create)
	return false;
      if (!handled_type (accesstype))
	return false;
      t = find_type (inner_type (inner_type (accesstype)));
      if (!t)
	t = record_type (inner_type (accesstype));
      if (!t || t->has_escaped ())
	return false;
      /* If base is not void* mark the type as escaping.
	 release INTEGER_TYPE cast to struct pointer.
	 (If t has escpaed above, then directly returns
	 and doesn't mark escape follow.). */
      /* _1 = MEM[(struct arc_t * *)a_1].
	 then base a_1: ssa_name  - pointer_type - integer_type.  */
      if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	{
	  bool is_int_ptr = POINTER_TYPE_P (TREE_TYPE (base))
			    && (TREE_CODE (inner_type (TREE_TYPE (base)))
				== INTEGER_TYPE);
	  if (!(VOID_POINTER_P (TREE_TYPE (base))
		|| (current_function->is_safe_func && is_int_ptr)))
	    {
	      gcc_assert (can_escape);
	      t->mark_escape (escape_cast_another_ptr, NULL);
	      return false;
	    }
	  if (TREE_CODE (base) == SSA_NAME
	      && !(current_function->is_safe_func && is_int_ptr))
	    {
	      /* Add a safe func mechanism.  */
	      if (!(current_function->is_safe_func
		    && is_from_void_ptr_parm (base)))
		/* Add auxiliary information of the multi-layer pointer
		     type.  */
		current_function->record_decl (t, base, -1,
				isptrptr (accesstype) ? accesstype : NULL);
	    }
	}
      else
	{
	  if (!(VOID_POINTER_P (TREE_TYPE (base))))
	    {
	      gcc_assert (can_escape);
	      t->mark_escape (escape_cast_another_ptr, NULL);
	      return false;
	    }
	  if (TREE_CODE (base) == SSA_NAME)
	    {
	      /* Add auxiliary information of the multi-layer pointer
		 type.  */
	      current_function->record_decl (t, base, -1,
			isptrptr (accesstype) ? accesstype : NULL);
	    }
	}
    }
  else if (!d)
    return false;
  else
    t = d->type;

  if (t->has_escaped ())
  {
    escape_from_base = true;
    return false;
  }

  if (mark_as_bit_field)
    {
      gcc_assert (can_escape);
      t->mark_escape (escape_bitfields, NULL);
      return false;
    }

  /* Escape the operation of fetching field with pointer offset such as:
     *(&(t->right)) = malloc (0); -> MEM[(struct node * *)_1 + 8B] = malloc (0);
  */
  if (current_layout_opt_level > STRUCT_SPLIT
      && (TREE_CODE (expr) == MEM_REF) && (offset != 0))
    {
      gcc_assert (can_escape);
      t->mark_escape (escape_non_multiply_size, NULL);
      return false;
    }

  if (wholeaccess (expr, base, accesstype, t))
    {
      field = NULL;
      type = t;
      indirect = accesstype != NULL;
      return true;
    }

  srfield *f = NULL;
  if (TREE_CODE (expr) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (expr, 1)))
    /* Static field compression may create bitfield.  In this case,
       byte position is not reliable.  */
    f = t->find_field_by_decl (TREE_OPERAND (expr, 1));
  else
    f = t->find_field (offset);
  if (!f)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nunkown field\n");
	  print_generic_expr (dump_file, expr);
	  fprintf (dump_file, "\n");
	  print_generic_expr (dump_file, base);
	}
      gcc_assert (can_escape);
      t->mark_escape (escape_unkown_field, NULL);
      return false;
    }
  if (!types_compatible_p (f->fieldtype, TREE_TYPE (expr)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nfieldtype = ");
	  print_generic_expr (dump_file, f->fieldtype);
	  fprintf (dump_file, "\naccess type = ");
	  print_generic_expr (dump_file, TREE_TYPE (expr));
	  fprintf (dump_file, "\noriginal expr = ");
	  print_generic_expr (dump_file, expr);
	}
      gcc_assert (can_escape);
      t->mark_escape (escape_unkown_field, NULL);
      return false;
    }
  field = f;
  type = t;
  indirect = accesstype != NULL;
  return true;
}

/* Mark the type used in EXPR as escaping.  */

void
ipa_struct_reorg::mark_expr_escape (tree expr, escape_type escapes,
				    gimple *stmt)
{
  tree base;
  bool indirect;
  srtype *type;
  srfield *field;
  bool realpart, imagpart, address;
  bool escape_from_base = false;
  if (!get_type_field (expr, base, indirect, type, field,
		       realpart, imagpart, address, escape_from_base))
    return;

  type->mark_escape (escapes, stmt);
}

/* Record accesses in a call statement STMT.  */

void
ipa_struct_reorg::maybe_record_call (cgraph_node *node, gcall *stmt)
{
  tree argtype;
  tree fndecl;
  escape_type escapes = does_not_escape;
  bool free_or_realloc = gimple_call_builtin_p (stmt, BUILT_IN_FREE)
			 || gimple_call_builtin_p (stmt, BUILT_IN_REALLOC);

  /* We check allocation sites in a different location.  */
  if (handled_allocation_stmt (stmt))
    return;

  /* A few cases here:
     1) assigned from the lhs
     2) Used in argument
     If a function being called is global (or indirect)
     then we reject the types as being escaping.  */

  if (tree chain = gimple_call_chain (stmt))
    record_stmt_expr (chain, node, stmt);

  /* Assigned from LHS.  */
  if (tree lhs = gimple_call_lhs (stmt))
    {
      /* FIXME: handle return types.  */
      mark_type_as_escape (TREE_TYPE (lhs), escape_return);
    }

  /* If we have an internal call, just record the stmt.  */
  if (gimple_call_internal_p (stmt))
    {
      for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
	record_stmt_expr (gimple_call_arg (stmt, i), node, stmt);
      return;
    }

  fndecl = gimple_call_fndecl (stmt);

  /* If we have an indrect call, just mark the types as escape.  */
  if (!fndecl)
    escapes = escape_pointer_function;
  /* Non local functions cause escape except for calls to free
     and realloc.
     FIXME: should support function annotations too.  */
  else if (!free_or_realloc
	   && !cgraph_node::local_info_node (fndecl)->local)
    escapes = escape_external_function;
  else if (!free_or_realloc
	   && !cgraph_node::local_info_node (fndecl)->can_change_signature)
    escapes = escape_cannot_change_signature;
  /* FIXME: we should be able to handle functions in other partitions.  */
  else if (symtab_node::get (fndecl)->in_other_partition)
    escapes = escape_external_function;

  if (escapes != does_not_escape)
    {
      for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
	{
	  mark_type_as_escape (TREE_TYPE (gimple_call_arg (stmt, i)),
			       escapes);
	  srdecl *d = current_function->find_decl (
					  gimple_call_arg (stmt, i));
	  if (d)
	    d->type->mark_escape (escapes, stmt);

	  if (escapes == escape_external_function
	      && !gimple_call_builtin_p (stmt, BUILT_IN_MEMSET))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "escape_external_function: ");
		  print_gimple_stmt (dump_file, stmt, 0);
		}
	      if (d)
		ext_func_types.safe_push (d->type);
	    }
	}
      return;
    }

  /* Get func param it's tree_list.  */
  argtype = TYPE_ARG_TYPES (gimple_call_fntype (stmt));
  for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
    {
      tree arg = gimple_call_arg (stmt, i);
      if (argtype)
	{
	  tree argtypet = TREE_VALUE (argtype);
	  /* callee_func (_1, _2);
	     Check the callee func, instead of current func.  */
	  if (!(free_or_realloc
		|| (current_layout_opt_level >= STRUCT_REORDER_FIELDS
		    && safe_functions.contains (
		       node->get_edge (stmt)->callee)))
	      && VOID_POINTER_P (argtypet))
	    mark_type_as_escape (TREE_TYPE (arg), escape_cast_void, stmt);
	  else
	    record_stmt_expr (arg, node, stmt);
	}
      else
	mark_type_as_escape (TREE_TYPE (arg), escape_var_arg_function);

      argtype = argtype ? TREE_CHAIN (argtype) : NULL_TREE;
    }

  /* Types escapes via a argument at empty or inlined function.  */
  cgraph_node *callee = node->get_edge (stmt)->callee;
  if (!gimple_call_builtin_p (stmt, BUILT_IN_FREE)
      && gimple_call_num_args (stmt)
      && callee && (!callee->has_gimple_body_p () || callee->inlined_to))
    {
      for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
	mark_type_as_escape (TREE_TYPE (gimple_call_arg (stmt, i)),
			      escape_var_arg_function);
    }
}

void
ipa_struct_reorg::record_stmt_expr (tree expr, cgraph_node *node, gimple *stmt)
{
  tree base;
  bool indirect;
  srtype *type;
  srfield *field;
  bool realpart, imagpart, address;
  bool escape_from_base = false;
  if (!get_type_field (expr, base, indirect, type, field, realpart,
		       imagpart, address, escape_from_base, false, true))
    return;

  if (current_layout_opt_level > NONE)
    {
      if (!opt_for_fn (current_function_decl, flag_ipa_struct_reorg))
	type->mark_escape (escape_non_optimize, stmt);
    }

  /* Record it.  */
  type->add_access (new sraccess (expr, stmt, node, find_function (node),
				  type, base, field));
}

/* Find function corresponding to NODE.  */

srfunction *
ipa_struct_reorg::find_function (cgraph_node *node)
{
  for (unsigned i = 0; i < functions.length (); i++)
    {
      if (functions[i]->node == node)
	return functions[i];

      srfunction *cloned_func = functions[i]->fc_path.cloned_func;
      if (current_fc_level == fc_level::DYNAMIC
	  && cloned_func && cloned_func->node == node)
	return cloned_func;
    }
  return NULL;
}

void
ipa_struct_reorg::check_type_and_push (tree newdecl, srdecl *decl,
				       vec<srdecl *> &worklist, gimple *stmt)
{
  srtype *type = decl->type;
  if (integer_zerop (newdecl))
    return;

  if (TREE_CODE (newdecl) == ADDR_EXPR)
    {
      srdecl *d = find_decl (TREE_OPERAND (newdecl, 0));
      if (!d)
	{
	  type->mark_escape (escape_cast_another_ptr, stmt);
	  return;
	}
      if (d->type == type
	  && cmp_ptr_layers (TREE_TYPE (newdecl), TREE_TYPE (decl->decl)))
	return;

      srtype *type1 = d->type;
      type->mark_escape (escape_cast_another_ptr, stmt);
      type1->mark_escape (escape_cast_another_ptr, stmt);
      return;
    }

  srdecl *d = find_decl (newdecl);
  if (!d)
    {
      if (TREE_CODE (newdecl) == INTEGER_CST)
	{
	  type->mark_escape (escape_int_const, stmt);
	  return;
	}
      /* If we have a non void* or a decl (which is hard to track),
	 then mark the type as escaping.  */
      if (replace_type_map.get (type->type) == NULL
	  && (!VOID_POINTER_P (TREE_TYPE (newdecl))
	      || DECL_P (newdecl)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\nunkown decl: ");
	      print_generic_expr (dump_file, newdecl);
	      fprintf (dump_file, " in type:\n");
	      print_generic_expr (dump_file, TREE_TYPE (newdecl));
	      fprintf (dump_file, "\n");
	    }
	  type->mark_escape (escape_cast_another_ptr, stmt);
	  return;
	}
      /* At this point there should only be unkown void* ssa names.  */
      gcc_assert (TREE_CODE (newdecl) == SSA_NAME);
      tree inner = SSA_NAME_VAR (newdecl);
      if (current_layout_opt_level >= STRUCT_REORDER_FIELDS && 
	  inner && find_decl (inner) == NULL)
	{
	  type->mark_escape (escape_no_record_var, stmt);
	  return;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nrecording unkown decl: ");
	  print_generic_expr (dump_file, newdecl);
	  fprintf (dump_file, " as type:\n");
	  type->simple_dump (dump_file);
	  fprintf (dump_file, "\n");
	}
      d = current_function->record_decl (type, newdecl, -1);
      worklist.safe_push (d);
      return;
    }

  /* Only add to the worklist if the decl is a SSA_NAME.  */
  if (TREE_CODE (newdecl) == SSA_NAME)
    worklist.safe_push (d);
  tree a_decl = d->orig_type ? d->orig_type : TREE_TYPE (newdecl);
  tree b_decl = decl->orig_type ? decl->orig_type : TREE_TYPE (decl->decl);
  if (d->type == type && cmp_ptr_layers (a_decl, b_decl))
    return;

  srtype *type1 = d->type;
  type->mark_escape (escape_cast_another_ptr, stmt);
  type1->mark_escape (escape_cast_another_ptr, stmt);
}

void
ipa_struct_reorg::check_alloc_num (gimple *stmt, srtype *type, bool ptrptr)
{
  if (current_layout_opt_level >= COMPLETE_STRUCT_RELAYOUT
      && handled_allocation_stmt (stmt))
    {
      tree arg0 = gimple_call_arg (stmt, 0);
      basic_block bb = gimple_bb (stmt);
      cgraph_node *node = current_function->node;
      if (!ptrptr && current_layout_opt_level >= POINTER_COMPRESSION_SAFE
	  && gimple_call_builtin_p (stmt, BUILT_IN_MALLOC))
	{
	  /* Malloc is commonly used for allocations of
	  a single struct, it is no meaning to do pointer
	  compression, and semi-relayout will waste a mess
	  of memory, so we skip it.  */
	  type->has_alloc_array = -4;
	  return;
	}
      if (integer_onep (arg0))
	/* Actually NOT an array, but may ruin other array.  */
	type->has_alloc_array = -1;
      else if (bb->loop_father != NULL
	       && loop_outer (bb->loop_father) != NULL)
	{
	  /* For semi-relayout, do not escape realloc.  */
	  if (current_layout_opt_level & SEMI_RELAYOUT
	      && gimple_call_builtin_p (stmt, BUILT_IN_REALLOC))
	    return;
	  /* The allocation is in a loop.  */
	  type->has_alloc_array = -2;
	}
      else if (node->callers != NULL)
	type->has_alloc_array = -3;
      else
	type->has_alloc_array = type->has_alloc_array < 0
				  ? type->has_alloc_array
				  : type->has_alloc_array + 1;

      if (current_layout_opt_level & POINTER_COMPRESSION_SAFE
	  && TREE_CODE (arg0) == INTEGER_CST)
	{
	  /* Only known size during compilation can be optimized
	     at this level.  */
	  unsigned HOST_WIDE_INT max_alloc_size = 0;
	  switch (compressed_size)
	    {
	      case 8: max_alloc_size = 0xff; break; // max of uint8
	      case 16: max_alloc_size = 0xffff; break; // max of uint16
	      case 32: max_alloc_size = 0xffffffff; break; // max of uint32
	      default: gcc_unreachable (); break;
	    }
	  if (tree_to_uhwi (arg0) < max_alloc_size)
	    type->has_legal_alloc_num = true;
	}
    }
}

/* Check the definition of gimple assign.  */

void
ipa_struct_reorg::check_definition_assign (srdecl *decl,
					   vec<srdecl *> &worklist)
{
  tree ssa_name = decl->decl;
  srtype *type = decl->type;
  gimple *stmt = SSA_NAME_DEF_STMT (ssa_name);
  gcc_assert (gimple_code (stmt) == GIMPLE_ASSIGN);
  /* a) if the SSA_NAME is sourced from a pointer plus, record the pointer and
	check to make sure the addition was a multiple of the size.
	check the pointer type too.  */
  tree rhs = gimple_assign_rhs1 (stmt);
  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    {
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree num = NULL_TREE;
      /* Specify the correct size for the multi-layer pointer.  */
      if (!is_result_of_mult (rhs2, &num, isptrptr (decl->orig_type)
					  ? TYPE_SIZE_UNIT (decl->orig_type)
					  : TYPE_SIZE_UNIT (type->type)))
	type->mark_escape (escape_non_multiply_size, stmt);

      if (TREE_CODE (rhs) == SSA_NAME)
	check_type_and_push (rhs, decl, worklist, stmt);
      return;
    }

  if (gimple_assign_rhs_code (stmt) == MAX_EXPR
      || gimple_assign_rhs_code (stmt) == MIN_EXPR
      || gimple_assign_rhs_code (stmt) == BIT_IOR_EXPR
      || gimple_assign_rhs_code (stmt) == BIT_XOR_EXPR
      || gimple_assign_rhs_code (stmt) == BIT_AND_EXPR)
    {
      tree rhs2 = gimple_assign_rhs2 (stmt);
      if (TREE_CODE (rhs) == SSA_NAME)
	check_type_and_push (rhs, decl, worklist, stmt);
      if (TREE_CODE (rhs2) == SSA_NAME)
	check_type_and_push (rhs2, decl, worklist, stmt);
      return;
    }

  /* Casts between pointers and integer are escaping.  */
  if (gimple_assign_cast_p (stmt))
    {
      if (current_layout_opt_level != COMPLETE_STRUCT_RELAYOUT
	  || replace_type_map.get (type->type) == NULL)
	type->mark_escape (escape_cast_int, stmt);
      return;
    }

  if (semi_relayout_map.get (type->type) != NULL)
    {
      if (current_layout_opt_level != COMPLETE_STRUCT_RELAYOUT)
	type->mark_escape (escape_unhandled_rewrite, stmt);
      return;
    }

  /* d) if the name is from a cast/assignment, make sure it is used as
	that type or void*
	i) If void* then push the ssa_name into worklist.  */
  gcc_assert (gimple_assign_single_p (stmt));
  check_other_side (decl, rhs, stmt, worklist);
  check_ptr_layers (decl->decl, rhs, stmt);
}

/* Check the definition of gimple call.  */

void
ipa_struct_reorg::check_definition_call (srdecl *decl, vec<srdecl *> &worklist)
{
  tree ssa_name = decl->decl;
  srtype *type = decl->type;
  gimple *stmt = SSA_NAME_DEF_STMT (ssa_name);
  gcc_assert (gimple_code (stmt) == GIMPLE_CALL);

  /* For realloc, check the type of the argument.  */
  if (gimple_call_builtin_p (stmt, BUILT_IN_REALLOC))
    check_type_and_push (gimple_call_arg (stmt, 0), decl, worklist, stmt);

  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
    {
      if (!handled_allocation_stmt (stmt))
	type->mark_escape (escape_return, stmt);
      if (!allocate_size (type, decl, stmt))
	type->mark_escape (escape_non_multiply_size, stmt);
    }
  else
    {
      if (!handled_allocation_stmt (stmt)
	  || !allocate_size (type, decl, stmt))
	type->mark_escape (escape_return, stmt);
    }

  bool ptrptr = isptrptr (decl->orig_type);
  check_alloc_num (stmt, type, ptrptr);
  return;
}

/*
  2) Check SSA_NAMEs for non type usages (source or use) (worlist of srdecl)
     a) if the SSA_NAME is sourced from a pointer plus, record the pointer and
	check to make sure the addition was a multiple of the size.
	check the pointer type too.
     b) If the name is sourced from an allocation check the allocation
	i) Add SSA_NAME (void*) to the worklist if allocated from realloc
     c) if the name is from a param, make sure the param type was of the
	original type
     d) if the name is from a cast/assignment, make sure it is used as that
	type or void*
	i) If void* then push the ssa_name into worklist
*/
void
ipa_struct_reorg::check_definition (srdecl *decl, vec<srdecl *> &worklist)
{
  tree ssa_name = decl->decl;
  srtype *type = decl->type;

  /*
     c) if the name is from a param, make sure the param type was of the
	original type.
  */
  if (SSA_NAME_IS_DEFAULT_DEF (ssa_name))
    {
      tree var = SSA_NAME_VAR (ssa_name);
      if (var
	  && TREE_CODE (var) == PARM_DECL
	  && VOID_POINTER_P (TREE_TYPE (ssa_name)))
	type->mark_escape (escape_cast_void, SSA_NAME_DEF_STMT (ssa_name));
      return;
    }
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS
      && SSA_NAME_VAR (ssa_name)
      && VOID_POINTER_P (TREE_TYPE (SSA_NAME_VAR (ssa_name))))
      {
	if (current_layout_opt_level < POINTER_COMPRESSION_SAFE
	  || !safe_void_cmp_p (ssa_name, type))
	  {
	    type->mark_escape (escape_cast_void, SSA_NAME_DEF_STMT (ssa_name));
	  }
      }
  gimple *stmt = SSA_NAME_DEF_STMT (ssa_name);

  /*
     b) If the name is sourced from an allocation check the allocation
	i) Add SSA_NAME (void*) to the worklist if allocated from realloc
  */
  if (gimple_code (stmt) == GIMPLE_CALL)
    check_definition_call (decl, worklist);
  /* If the SSA_NAME is sourced from an inline-asm,
     just mark the type as escaping.  */
  if (gimple_code (stmt) == GIMPLE_ASM)
    {
      type->mark_escape (escape_inline_asm, stmt);
      return;
    }

  /* If the SSA_NAME is sourced from a PHI check add
     each name to the worklist and check to make sure
     they are used correctly.  */
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
	check_type_and_push (gimple_phi_arg_def (stmt, i),
			     decl, worklist, stmt);
      return;
    }
  if (gimple_code (stmt) == GIMPLE_ASSIGN)
    check_definition_assign (decl, worklist);
}

/* Mark the types used by the inline-asm as escaping.
   It is unkown what happens inside an inline-asm.  */

void
ipa_struct_reorg::mark_types_asm (gasm *astmt)
{
  for (unsigned i = 0; i < gimple_asm_ninputs (astmt); i++)
    {
      tree v = TREE_VALUE (gimple_asm_input_op (astmt, i));
      /* If we have &b, just strip the & here.  */
      if (TREE_CODE (v) == ADDR_EXPR)
	v = TREE_OPERAND (v, 0);
      mark_expr_escape (v, escape_inline_asm, astmt);
    }
  for (unsigned i = 0; i < gimple_asm_noutputs (astmt); i++)
    {
      tree v = TREE_VALUE (gimple_asm_output_op (astmt, i));
      /* If we have &b, just strip the & here.  */
      if (TREE_CODE (v) == ADDR_EXPR)
	v = TREE_OPERAND (v, 0);
      mark_expr_escape (v, escape_inline_asm, astmt);
    }
}

void
ipa_struct_reorg::check_other_side (srdecl *decl, tree other, gimple *stmt,
				    vec<srdecl *> &worklist)
{
  srtype *type = decl->type;

  if (TREE_CODE (other) == SSA_NAME || DECL_P (other)
      || TREE_CODE (other) == INTEGER_CST)
    {
      check_type_and_push (other, decl, worklist, stmt);
      return;
    }

  tree t = TREE_TYPE (other);
  if (!handled_type (t))
    {
      type->mark_escape (escape_cast_another_ptr, stmt);
      return;
    }

  srtype *t1 = find_type (inner_type (t));
  /* In the other side check, escape mark is added
     when the replacement struct type exists. */
  if (t1 == type || is_replace_type (inner_type (t), type->type))
    {
      /* In Complete Struct Relayout, if lhs type is the same
	 as rhs type, we could return without any harm.  */
      if (current_layout_opt_level == COMPLETE_STRUCT_RELAYOUT)
	return;

      tree base;
      bool indirect;
      srtype *type1;
      srfield *field;
      bool realpart, imagpart, address;
      bool escape_from_base = false;
      if (!get_type_field (other, base, indirect, type1, field,
			   realpart, imagpart, address, escape_from_base))
	{
	  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	    {
	      /* Release INTEGER_TYPE cast to struct pointer.  */
	      bool cast_from_int_ptr = current_function->is_safe_func && base
		&& find_decl (base) == NULL && POINTER_TYPE_P (TREE_TYPE (base))
		&& (TREE_CODE (inner_type (TREE_TYPE (base))) == INTEGER_TYPE);

	      /* Add a safe func mechanism.  */
	      bool from_void_ptr_parm = current_function->is_safe_func
		&& TREE_CODE (base) == SSA_NAME && is_from_void_ptr_parm (base);

	      /* Release type is used by a type which escapes.  */
	      if (escape_from_base || cast_from_int_ptr || from_void_ptr_parm)
		return;
	    }
	  type->mark_escape (escape_cast_another_ptr, stmt);
	}

      return;
    }

  if (t1)
    t1->mark_escape (escape_cast_another_ptr, stmt);

  type->mark_escape (escape_cast_another_ptr, stmt);
}


/* Get the expr base.  */

void
get_base (tree &base, tree expr)
{
  if (TREE_CODE (expr) == MEM_REF)
    base = TREE_OPERAND (expr, 0);
  else if (TREE_CODE (expr) == COMPONENT_REF)
    {
      base = TREE_OPERAND (expr, 0);
      base = (TREE_CODE (base) == MEM_REF) ? TREE_OPERAND (base, 0) : base;
    }
  else if (TREE_CODE (expr) == ADDR_EXPR)
    base = TREE_OPERAND (expr, 0);
}

/* Check whether the number of pointer layers of exprs is equal,
   marking unequals as escape.  */

void
ipa_struct_reorg::check_ptr_layers (tree a_expr, tree b_expr, gimple *stmt)
{
  if (current_layout_opt_level < STRUCT_REORDER_FIELDS
      || current_function->is_safe_func
      || !(POINTER_TYPE_P (TREE_TYPE (a_expr)))
      || !(POINTER_TYPE_P (TREE_TYPE (b_expr)))
      || !handled_type (TREE_TYPE (a_expr))
      || !handled_type (TREE_TYPE (b_expr)))
    return;

  tree a_base = a_expr;
  tree b_base = b_expr;
  get_base (a_base, a_expr);
  get_base (b_base, b_expr);

  srdecl *a = find_decl (a_base);
  srdecl *b = find_decl (b_base);
  if (a && b == NULL && TREE_CODE (b_expr) != INTEGER_CST)
    {
      a->type->mark_escape (escape_cast_another_ptr, stmt);
      return;
    }
  else if (b && a == NULL && TREE_CODE (a_expr) != INTEGER_CST)
    {
      b->type->mark_escape (escape_cast_another_ptr, stmt);
      return;
    }
  else if (a == NULL && b == NULL)
    return;

  if (cmp_ptr_layers (TREE_TYPE (a_expr), TREE_TYPE (b_expr)))
    return;

  if (a)
    a->type->mark_escape (escape_cast_another_ptr, stmt);
  if (b)
    b->type->mark_escape (escape_cast_another_ptr, stmt);
}

void
ipa_struct_reorg::check_use (srdecl *decl, gimple *stmt,
			     vec<srdecl *> &worklist)
{
  srtype *type = decl->type;

  if (gimple_code (stmt) == GIMPLE_RETURN)
    {
      type->mark_escape (escape_return, stmt);
      return;
    }
  /* If the SSA_NAME PHI check and add the src to the worklist and
     check to make sure they are used correctly.  */
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      check_type_and_push (gimple_phi_result (stmt), decl, worklist, stmt);
      return;
    }

  if (gimple_code (stmt) == GIMPLE_ASM)
    {
      mark_types_asm (as_a <gasm *> (stmt));
      return;
    }

  if (gimple_code (stmt) == GIMPLE_COND)
    {
      tree rhs1 = gimple_cond_lhs (stmt);
      tree rhs2 = gimple_cond_rhs (stmt);
      tree orhs = rhs1;
      enum tree_code code = gimple_cond_code (stmt);
      if ((current_layout_opt_level == STRUCT_SPLIT
	   && (code != EQ_EXPR && code != NE_EXPR))
	   || (current_layout_opt_level >= COMPLETE_STRUCT_RELAYOUT
	       && (code != EQ_EXPR && code != NE_EXPR
		   && code != LT_EXPR && code != LE_EXPR
		   && code != GT_EXPR && code != GE_EXPR)))
	{
	  mark_expr_escape (rhs1, escape_non_eq, stmt);
	  mark_expr_escape (rhs2, escape_non_eq, stmt);
	}
      if (rhs1 == decl->decl)
	orhs = rhs2;
      if (integer_zerop (orhs))
	return;
      if (TREE_CODE (orhs) != SSA_NAME)
	mark_expr_escape (rhs1, escape_non_eq, stmt);
      check_type_and_push (orhs, decl, worklist, stmt);
      return;
    }

  /* Casts between pointers and integer are escaping.  */
  if (gimple_assign_cast_p (stmt))
    {
      if (current_layout_opt_level != COMPLETE_STRUCT_RELAYOUT
	  || replace_type_map.get (type->type) == NULL)
	type->mark_escape (escape_cast_int, stmt);
      return;
    }

  /* We might have a_1 = ptr_2 == ptr_3; */
  if (is_gimple_assign (stmt)
      && TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_comparison)
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree orhs = rhs1;
      enum tree_code code = gimple_assign_rhs_code (stmt);
      if ((current_layout_opt_level == STRUCT_SPLIT
	   && (code != EQ_EXPR && code != NE_EXPR))
	   || (current_layout_opt_level >= COMPLETE_STRUCT_RELAYOUT
	       && (code != EQ_EXPR && code != NE_EXPR
		   && code != LT_EXPR && code != LE_EXPR
		   && code != GT_EXPR && code != GE_EXPR)))
	{
	  mark_expr_escape (rhs1, escape_non_eq, stmt);
	  mark_expr_escape (rhs2, escape_non_eq, stmt);
	}
      if (rhs1 == decl->decl)
	orhs = rhs2;
      if (integer_zerop (orhs))
	return;
      if (TREE_CODE (orhs) != SSA_NAME)
	mark_expr_escape (rhs1, escape_non_eq, stmt);
      check_type_and_push (orhs, decl, worklist, stmt);
      return;
    }

  if (gimple_assign_single_p (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs = gimple_assign_rhs1 (stmt);
      /* Check if we have a_1 = b_2; that a_1 is in the correct type.  */
      if (decl->decl == rhs)
	{
	  check_other_side (decl, lhs, stmt, worklist);
	  return;
	}
      check_ptr_layers (lhs, rhs, stmt);
    }

  if (is_gimple_assign (stmt)
      && gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    {
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree lhs = gimple_assign_lhs (stmt);
      tree num;
      check_other_side (decl, lhs, stmt, worklist);
      check_ptr_layers (lhs, decl->decl, stmt);
      /* Specify the correct size for the multi-layer pointer.  */
      if (!is_result_of_mult (rhs2, &num, isptrptr (decl->orig_type)
					  ? TYPE_SIZE_UNIT (decl->orig_type)
					  : TYPE_SIZE_UNIT (type->type)))
	type->mark_escape (escape_non_multiply_size, stmt);
    }

  if (is_gimple_assign (stmt)
      && gimple_assign_rhs_code (stmt) == POINTER_DIFF_EXPR)
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree other = rhs1 == decl->decl ? rhs2 : rhs1;

      check_other_side (decl, other, stmt, worklist);
      check_ptr_layers (decl->decl, other, stmt);
      return;
    }

}

/*
  2) Check SSA_NAMEs for non type usages (source or use) (worlist of srdecl)
     d) if the name is from a cast/assignment, make sure it is used as that
	type or void*
	i) If void* then push the ssa_name into worklist
     e) if used in conditional check the other side
	i) If the conditional is non NE/EQ then mark the type as non rejecting
     f) Check if the use in a Pointer PLUS EXPR Is used by mulitplication
	of its size
  */
void
ipa_struct_reorg::check_uses (srdecl *decl, vec<srdecl *> &worklist)
{
  tree ssa_name = decl->decl;
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, ssa_name)
    {
      gimple *stmt = USE_STMT (use_p);

      if (is_gimple_debug (stmt))
	continue;

      check_use (decl, stmt, worklist);
    }
}

/* Record function corresponding to NODE.  */

srfunction *
ipa_struct_reorg::record_function (cgraph_node *node, srfunction *sfn)
{
  function *fn;
  tree parm, var;
  unsigned int i;
  escape_type escapes = does_not_escape;

  if (dump_file  && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "\nRecording accesses and types from function: %s/%u\n",
	     node->name (), node->order);

  /* Nodes without a body are not interesting.  Especially do not
     visit clones at this point for now - we get duplicate decls
     there for inline clones at least.  */
  if (!node->has_gimple_body_p () || node->inlined_to)
    return sfn;

  node->get_body ();
  fn = DECL_STRUCT_FUNCTION (node->decl);

  if (!fn)
    return sfn;

  if (!sfn)
    {
      sfn = new srfunction (node);
      functions.safe_push (sfn);
    }

  current_function = sfn;

  if (DECL_PRESERVE_P (node->decl))
    escapes = escape_marked_as_used;
  else if (!node->local)
    {
      if (current_layout_opt_level < STRUCT_REORDER_FIELDS)
	escapes = escape_visible_function;
      else if (node->externally_visible)
	escapes = escape_visible_function;
    }
  else if (!node->can_change_signature)
    escapes = escape_cannot_change_signature;
  else if (!tree_versionable_function_p (node->decl))
    escapes = escape_noclonable_function;

  if (current_layout_opt_level > NONE)
    {
      if (!opt_for_fn (node->decl, flag_ipa_struct_reorg))
	escapes = escape_non_optimize;
    }

  basic_block bb;
  gimple_stmt_iterator si;

  /* Add a safe func mechanism.  */
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
    {
      current_function->is_safe_func = safe_functions.contains (node);
      if (dump_file  && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nfunction %s/%u: is_safe_func = %d\n",
		   node->name (), node->order,
		   current_function->is_safe_func);
	}
    }

  /* Record the static chain decl.  */
  if (fn->static_chain_decl)
   {
     srdecl *sd = record_var (fn->static_chain_decl,
			      escapes, -2);
      if (sd)
	{
	  /* Specify that this type is used by the static
	     chain so it cannot be split.  */
	  sd->type->chain_type = true;
	  sfn->add_arg (sd);
	  sd->type->add_function (sfn);
	}
    }

  /* Record the arguments.  */
  for (parm = DECL_ARGUMENTS (node->decl), i = 0;
       parm;
       parm = DECL_CHAIN (parm), i++)
   {
      srdecl *sd = record_var (parm, escapes, i);
      if (sd)
	{
	  sfn->add_arg (sd);
	  sd->type->add_function (sfn);
	}
    }

  /* Mark the return type as escaping.  */
  {
    tree return_type = TREE_TYPE (TREE_TYPE (node->decl));
    mark_type_as_escape (return_type, escape_return, NULL);
  }

  /* If the cfg does not exist for the function,
     don't process the function.  */
  if (!fn->cfg)
    {
      current_function = NULL;
      return sfn;
    }

  /* The following order is done for recording stage:
     0) Record all variables/SSA_NAMES that are of struct type
     1) Record MEM_REF/COMPONENT_REFs
	a) Record SSA_NAMEs (void*) and record that as the accessed type.
  */

  push_cfun (fn);

  FOR_EACH_LOCAL_DECL (cfun, i, var)
    {
      if (TREE_CODE (var) != VAR_DECL)
	continue;

      record_var (var);
    }

  for (i = 1; i < num_ssa_names; ++i)
    {
      tree name = ssa_name (i);
      if (!name
	  || has_zero_uses (name)
	  || virtual_operand_p (name))
	continue;

      record_var (name);
    }

  /* Find the variables which are used via MEM_REF and are void* types.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  find_vars (stmt);
	}
    }

  auto_vec<srdecl *> worklist;
  for (unsigned i = 0; i < current_function->decls.length (); i++)
    {
      srdecl *decl = current_function->decls[i];
      if (TREE_CODE (decl->decl) == SSA_NAME)
	{
	  decl->visited = false;
	  worklist.safe_push (decl);
	}
    }

/*
  2) Check SSA_NAMEs for non type usages (source or use) (worlist of srdecl)
     a) if the SSA_NAME is sourced from a pointer plus, record the pointer and
	check to make sure the addition was a multiple of the size.
	check the pointer type too.
     b) If the name is sourced from an allocation check the allocation
	i) Add SSA_NAME (void*) to the worklist if allocated from realloc
     c) if the name is from a param, make sure the param type was of the
	original type
     d) if the name is used in a cast/assignment, make sure it is used as that
	type or void*
	i) If void* then push the ssa_name into worklist
     e) if used in conditional check the other side
	i) If the conditional is non NE/EQ then mark the type as non rejecting
     f) Check if the use in a POinter PLUS EXPR Is used by mulitplication
	of its size
*/

  while (!worklist.is_empty ())
    {
      srdecl *decl = worklist.pop ();
      if (decl->visited)
	continue;
      decl->visited = true;
      check_definition (decl, worklist);
      check_uses (decl, worklist);
    }

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  maybe_record_stmt (node, stmt);
	}
    }

  pop_cfun ();
  current_function = NULL;
  return sfn;
}


/* For a function that contains the void* parameter and passes the structure
   pointer, check whether the function uses the input node safely.
   For these functions, the void* parameter and related ssa_name are not
   recorded in record_function (), and the input structure type is not escaped.
*/

void
ipa_struct_reorg::record_safe_func_with_void_ptr_parm ()
{
  cgraph_node *node = NULL;
  FOR_EACH_FUNCTION (node)
    {
      if (!node->real_symbol_p ())
	continue;
      if (node->definition)
	{
	  if (!node->has_gimple_body_p () || node->inlined_to)
	    continue;
	  node->get_body ();
	  function *fn = DECL_STRUCT_FUNCTION (node->decl);
	  if (!fn)
	    continue;
	  push_cfun (fn);
	  if (is_safe_func_with_void_ptr_parm (node))
	    {
	      safe_functions.add (node);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\nfunction %s/%u is safe function.\n",
			 node->name (), node->order);
	    }
	  pop_cfun ();
	}
    }
}

/* Record all accesses for all types including global variables.  */

void
ipa_struct_reorg::record_accesses (void)
{
  varpool_node *var;
  cgraph_node *cnode;

  /* Record global (non-auto) variables first.  */
  FOR_EACH_VARIABLE (var)
    {
      if (!var->real_symbol_p ())
	continue;

      /* Record all variables including the accesses inside a variable.  */
      escape_type escapes = does_not_escape;
      if (var->externally_visible || !var->definition)
	escapes = escape_via_global_var;
      if (var->in_other_partition)
	escapes = escape_via_global_var;
      if (!var->externally_visible && var->definition)
	var->get_constructor ();
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Recording global variable: ");
	  print_generic_expr (dump_file, var->decl);
	  fprintf (dump_file, "\n");
	}
      record_var (var->decl, escapes);
    }

  /* Add a safe func mechanism.  */
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
    record_safe_func_with_void_ptr_parm ();

  FOR_EACH_FUNCTION (cnode)
    {
      if (!cnode->real_symbol_p ())
	continue;

      /* Record accesses inside a function.  */
      if (cnode->definition)
	record_function (cnode);
      else
	{
	  if (cnode->externally_visible)
	    {
	      tree return_type = TREE_TYPE (TREE_TYPE (cnode->decl));
	      mark_type_as_escape (return_type, escape_return, NULL);
	    }
	}

    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\n");
      fprintf (dump_file, "==============================================\n\n");
      fprintf (dump_file, "======== all types (before pruning): ========\n\n");
      dump_types (dump_file);
      fprintf (dump_file, "======= all functions (before pruning): =======\n");
      dump_functions (dump_file);
    }
  /* If record_var () is called later, new types will not be recorded.  */
  done_recording = true;
}

/* A helper function to detect cycles (recusive) types.
   Return TRUE if TYPE was a rescusive type.  */

bool
ipa_struct_reorg::walk_field_for_cycles (srtype *type)
{
  unsigned i;
  srfield *field;

  type->visited = true;
  if (type->escaped_rescusive ())
    return true;

  if (type->has_escaped ())
    return false;

  FOR_EACH_VEC_ELT (type->fields, i, field)
    {
      if (!field->type)
	;
      /* If there are two members of the same structure pointer type? */
      else if (field->type->visited
	       || walk_field_for_cycles (field->type))
	{
	  type->mark_escape (escape_rescusive_type, NULL);
	  return true;
	}
    }

  return false;
}

/* Clear visited on all types.  */

void
ipa_struct_reorg::clear_visited (void)
{
  for (unsigned i = 0; i < types.length (); i++)
    types[i]->visited = false;
}

/* Detect recusive types and mark them as escaping.  */

void
ipa_struct_reorg::detect_cycles (void)
{
  for (unsigned i = 0; i < types.length (); i++)
    {
      if (types[i]->has_escaped ())
	continue;

      clear_visited ();
      walk_field_for_cycles (types[i]);
    }
}

/* Propagate escaping to depdenent types.  */

void
ipa_struct_reorg::propagate_escape (void)
{
  unsigned i;
  srtype *type;
  bool changed = false;

  do
    {
      changed = false;
      FOR_EACH_VEC_ELT (types, i, type)
	{
	  for (tree field = TYPE_FIELDS (type->type);
	       field;
	       field = DECL_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL
		  && handled_type (TREE_TYPE (field)))
		{
		  tree t = inner_type (TREE_TYPE (field));
		  srtype *type1 = find_type (t);
		  if (!type1)
		    continue;
		  if (type1->has_escaped ()
		      && !type->has_escaped ())
		    {
		      type->mark_escape (escape_dependent_type_escapes, NULL);
		      changed = true;
		    }
		  if (type->has_escaped ()
		      && !type1->has_escaped ())
		    {
		      type1->mark_escape (escape_dependent_type_escapes, NULL);
		      changed = true;
		    }
		}
	    }
	}
    } while (changed);
}

/* If the original type (with members) has escaped, corresponding to the
   struct pointer type (empty member) in the structure fields
   should also marked as escape.  */

void
ipa_struct_reorg::propagate_escape_via_original (void)
{
  for (unsigned i = 0; i < types.length (); i++)
    {
      for (unsigned j = 0; j < types.length (); j++)
      {
	const char *type1 = get_type_name (types[i]->type);
	const char *type2 = get_type_name (types[j]->type);
	if (type1 == NULL || type2 == NULL)
	  continue;
	if (type1 == type2 && types[j]->has_escaped ())
	  {
	    if (!types[i]->has_escaped ())
	      types[i]->mark_escape (escape_via_orig_escape, NULL);
	    break;
	  }
      }
    }
}

/* Marks the fileds as empty and does not have the original structure type
   is escape.  */

void
ipa_struct_reorg::propagate_escape_via_empty_with_no_original (void)
{
  for (unsigned i = 0; i < types.length (); i++)
    {
      if (types[i]->fields.length () == 0)
	{
	  for (unsigned j = 0; j < types.length (); j++)
	    {
	      if (i != j && types[j]->fields.length ())
		{
		  const char *type1 = get_type_name (types[i]->type);
		  const char *type2 = get_type_name (types[j]->type);
		  if (type1 != NULL && type2 != NULL && type1 == type2)
		    break;
		}
	      if (j == types.length () - 1)
		types[i]->mark_escape (escape_via_empty_no_orig, NULL);
	    }
	}
    }
}

/* Escape propagation is performed on types that escape through external
   functions.  */

void
ipa_struct_reorg::propagate_escape_via_ext_func_types (void)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n propagate_escape_via_ext_func_types: \n\n");
  unsigned i = 0;
  hash_set<srtype *> visited_types;
  while (i < ext_func_types.length ())
    {
      visited_types.add (ext_func_types[i]);
      unsigned j = 0;
      srfield * field;
      FOR_EACH_VEC_ELT (ext_func_types[i]->fields, j, field)
	{
	  if (field->type)
	    {
	      if (!field->type->has_escaped ())
		field->type->mark_escape (escape_dependent_type_escapes, NULL);
	      if (!visited_types.contains (field->type))
		ext_func_types.safe_push (field->type);
	    }
	}
      i++;
    }
}

/* Escape propagation is performed on ssa_name decl that no record var in
   decls.  */

void
ipa_struct_reorg::propagate_escape_via_no_record_var (void)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n propagate_escape_via_no_record_var: \n\n");

  for (unsigned i = 0; i < functions.length (); i++)
    {
      if (functions[i]->node)
	set_cfun (DECL_STRUCT_FUNCTION (functions[i]->node->decl));

      for (unsigned j = 0; j < functions[i]->decls.length (); j++)
	{
	  srdecl *decl = functions[i]->decls[j];
	  srtype *type = decl->type;

	  if (TREE_CODE (decl->decl) == SSA_NAME)
	    {
	      tree inner = SSA_NAME_VAR (decl->decl);

	      if (inner && functions[i]->find_decl (inner) == NULL)
		type->mark_escape (escape_no_record_var, NULL);
	    }
	}

      set_cfun (NULL);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n end propagate_escape_via_no_record_var \n\n");
}

/* Prune the escaped types and their decls from what was recorded.  */

void
ipa_struct_reorg::prune_escaped_types (void)
{
  if (current_layout_opt_level == STRUCT_SPLIT)
    {
      /* Detect recusive types and mark them as escaping.  */
      detect_cycles ();
      /* If contains or is contained by the escape type,
	 mark them as escaping.  */
      propagate_escape ();
      propagate_escape_via_no_record_var ();
    }
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
    {
      propagate_escape_via_original ();
      propagate_escape_via_empty_with_no_original ();
      propagate_escape_via_ext_func_types ();
      propagate_escape_via_no_record_var ();
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "==============================================\n\n");
      fprintf (dump_file, "all types (after prop but before pruning): \n\n");
      dump_types (dump_file);
      fprintf (dump_file, "all functions (after prop but before pruning): \n");
      dump_functions (dump_file);
    }

  if (dump_file)
    dump_types_escaped (dump_file);

  /* Prune the function arguments which escape
     and functions which have no types as arguments.  */
  for (unsigned i = 0; i < functions.length ();)
    {
      srfunction *function = functions[i];
      prune_function (function);

      /* Prune functions which don't refer to any variables any more.  */
      if (function->args.is_empty ()
	  && function->decls.is_empty ()
	  && function->globals.is_empty ()
	  && current_layout_opt_level < STRUCT_REORDER_FIELDS)
	{
	  delete function;
	  functions.ordered_remove (i);
	}
      else
	i++;
    }

  prune_globals ();

  /* Prune types that escape, all references to those types
     will have been removed in the above loops.  */
  /* The escape type is not deleted in current_layout_opt_level
     after STRUCT_REORDER_FIELDS, then the type that contains
     the escaped type fields can find complete information.  */
  if (current_layout_opt_level < STRUCT_REORDER_FIELDS)
    {
      for (unsigned i = 0; i < types.length ();)
	{
	  srtype *type = types[i];
	  if (type->has_escaped ())
	    {
	      /* All references to this type should have been removed now.  */
	      delete type;
	      types.ordered_remove (i);
	    }
	  else
	    i++;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "==============================================\n\n");
      fprintf (dump_file, "========= all types (after pruning): =========\n\n");
      dump_types (dump_file);
      fprintf (dump_file, "======== all functions (after pruning): ========\n");
      dump_functions (dump_file);
    }
}

/* Prune the decls in function SRFN.  */

void
ipa_struct_reorg::prune_function (srfunction *srfn)
{
  /* Prune function arguments of types that escape.  */
  for (unsigned i = 0; i < srfn->args.length ();)
    {
      if (srfn->args[i]->type->has_escaped ())
	srfn->args.ordered_remove (i);
      else
	i++;
    }

  /* Prune global variables that the function uses of types that escape.  */
  for (unsigned i = 0; i < srfn->globals.length ();)
    {
      if (srfn->globals[i]->type->has_escaped ())
	srfn->globals.ordered_remove (i);
      else
	i++;
    }

  /* Prune variables that the function uses of types that escape.  */
  for (unsigned i = 0; i < srfn->decls.length ();)
    {
      srdecl *decl = srfn->decls[i];
      if (decl->type->has_escaped ())
	{
	  srfn->decls.ordered_remove (i);
	  delete decl;
	}
      else
	i++;
    }
}

/* Prune globals of types that escape, all references to those decls
   will have been removed in the first loop.  */

void
ipa_struct_reorg::prune_globals ()
{
  for (unsigned i = 0; i < globals.decls.length ();)
    {
      srdecl *decl = globals.decls[i];
      if (decl->type->has_escaped ())
	{
	  globals.decls.ordered_remove (i);
	  delete decl;
	}
      else
	i++;
    }
}

/* Analyze all of the types.  */

void
ipa_struct_reorg::analyze_types (void)
{
  for (unsigned i = 0; i < types.length (); i++)
    {
      if (!types[i]->has_escaped ())
	types[i]->analyze ();
    }
}

/* Create all new types we want to create.  */

bool
ipa_struct_reorg::create_new_types (void)
{
  int newtypes = 0;
  clear_visited ();
  for (unsigned i = 0; i < types.length (); i++)
    newtypes += types[i]->create_new_type ();

  /* Some new types may not have been created at create_new_type (), so
     recreate new type for all struct fields.  */
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
    {
      for (unsigned i = 0; i < types.length (); i++)
	{
	  auto_vec <tree> *fields = fields_to_finish.get (types[i]->type);
	  if (fields)
	    {
	      for (unsigned j = 0; j < fields->length (); j++)
		{
		  tree field = (*fields)[j];
		  if (types[i]->pc_candidate)
		    {
		      TREE_TYPE (field)
			= make_unsigned_type (compressed_size);
		      SET_DECL_ALIGN (field, compressed_size);
		    }
		  else
		    {
		      TREE_TYPE (field)
			= reconstruct_complex_type (TREE_TYPE (field),
						    types[i]->newtype[0]);
		    }
		}
	    }
	}
      for (unsigned i = 0; i < types.length (); i++)
	layout_type (types[i]->newtype[0]);
    }

  if (dump_file)
    {
      if (newtypes)
	fprintf (dump_file, "\nNumber of structures to transform is %d\n",
		 newtypes);
      else
	fprintf (dump_file, "\nNo structures to transform.\n");
    }

  return newtypes != 0;
}

/* Create all the new decls except for the new arguments
   which create_new_functions would have created.  */

void
ipa_struct_reorg::create_new_decls (void)
{
  globals.create_new_decls ();
  for (unsigned i = 0; i < functions.length (); i++)
    functions[i]->create_new_decls ();
}

/* Create the new arguments for the function corresponding to NODE.  */

void
ipa_struct_reorg::create_new_args (cgraph_node *new_node)
{
  tree decl = new_node->decl;
  auto_vec<tree> params;
  push_function_arg_decls (&params, decl);
  vec<ipa_adjusted_param, va_gc> *adjs = NULL;
  vec_safe_reserve (adjs, params.length ());
  for (unsigned i = 0; i < params.length (); i++)
    {
      struct ipa_adjusted_param adj;
      tree parm = params[i];
      memset (&adj, 0, sizeof (adj));
      adj.base_index = i;
      adj.prev_clone_index = i;
      srtype *t = find_type (inner_type (TREE_TYPE (parm)));
      if (!t
	  || t->has_escaped ()
	  || !t->has_new_type ())
	{
	  adj.op = IPA_PARAM_OP_COPY;
	  vec_safe_push (adjs, adj);
	  continue;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Creating a new argument for: ");
	  print_generic_expr (dump_file, params[i]);
	  fprintf (dump_file, " in function: ");
	  print_generic_expr (dump_file, decl);
	  fprintf (dump_file, "\n");
	}
      adj.op = IPA_PARAM_OP_NEW;
      adj.param_prefix_index = IPA_PARAM_PREFIX_REORG;
      for (unsigned j = 0; j < max_split && t->newtype[j]; j++)
	{
	  adj.type = reconstruct_complex_type (TREE_TYPE (parm),
					       t->newtype[j]);
	  vec_safe_push (adjs, adj);
	}
    }
  ipa_param_body_adjustments *adjustments
    = new ipa_param_body_adjustments (adjs, decl);
  adjustments->modify_formal_parameters ();
  auto_vec<tree> new_params;
  push_function_arg_decls (&new_params, decl);
  unsigned veclen = vec_safe_length (adjs);
  for (unsigned i = 0; i < veclen; i++)
    {
      if ((*adjs)[i].op != IPA_PARAM_OP_NEW)
	continue;
      tree decl = params[(*adjs)[i].base_index];
      srdecl *d = find_decl (decl);
      if (!d)
	continue;
      unsigned j = 0;
      while (j < max_split && d->newdecl[j])
	j++;
      d->newdecl[j] = new_params[i];
    }

  function *fn = DECL_STRUCT_FUNCTION (decl);

  if (!fn->static_chain_decl)
    return;
  srdecl *chain = find_decl (fn->static_chain_decl);
  if (!chain)
    return;

  srtype *type = chain->type;
  tree orig_var = chain->decl;
  const char *tname = NULL;
  if (DECL_NAME (orig_var))
    tname = IDENTIFIER_POINTER (DECL_NAME (orig_var));
  gcc_assert (!type->newtype[1]);
  tree new_name = NULL;
  char *name = NULL;
  if (tname)
    {
      name = concat (tname, ".reorg.0", NULL);
      new_name = get_identifier (name);
      free (name);
    }
  tree newtype1 = reconstruct_complex_type (TREE_TYPE (orig_var),
					    type->newtype[0]);
  chain->newdecl[0] = build_decl (DECL_SOURCE_LOCATION (orig_var),
				  PARM_DECL, new_name, newtype1);
  copy_var_attributes (chain->newdecl[0], orig_var);
  fn->static_chain_decl = chain->newdecl[0];
}

/* Find the refered DECL in the current function or globals.
   If this is a global decl, record that as being used
   in the current function.  */

srdecl *
ipa_struct_reorg::find_decl (tree decl)
{
  srdecl *d;
  d = globals.find_decl (decl);
  if (d)
    {
      /* Record the global usage in the current function.  */
      if (!done_recording && current_function)
	{
	  bool add = true;
	  /* No reason to add it to the current function if it is
	     already recorded as such.  */
	  for (unsigned i = 0; i < current_function->globals.length (); i++)
	    {
	      if (current_function->globals[i] == d)
		{
		  add = false;
		  break;
		}
	    }
	  if (add)
	    current_function->globals.safe_push (d);
	}
      return d;
    }
  if (current_function)
    return current_function->find_decl (decl);
  return NULL;
}

/* Create new function clones for the cases where the arguments
   need to be changed.  */

void
ipa_struct_reorg::create_new_functions (void)
{
  for (unsigned i = 0; i < functions.length (); i++)
    {
      srfunction *f = functions[i];
      bool anyargchanges = false;
      cgraph_node *new_node;
      cgraph_node *node = f->node;
      if (f->old)
	continue;

      if (f->args.length () == 0)
	continue;

      for (unsigned j = 0; j < f->args.length (); j++)
	{
	  srdecl *d = f->args[j];
	  srtype *t = d->type;
	  if (t->has_new_type ())
	    anyargchanges = true;
	}
      if (!anyargchanges)
	continue;

      if (dump_file)
	{
	  fprintf (dump_file, "Creating a clone of function: ");
	  f->simple_dump (dump_file);
	  fprintf (dump_file, "\n");
	}
      statistics_counter_event (NULL, "Create new function", 1);
      new_node = node->create_version_clone_with_body (
				vNULL, NULL, NULL, NULL, NULL, "struct_reorg");
      new_node->can_change_signature = node->can_change_signature;
      new_node->make_local ();
      f->newnode = new_node;
      srfunction *n = record_function (new_node);
      current_function = n;
      n->old = f;
      f->newf = n;
      /* Create New arguments.  */
      create_new_args (new_node);
      current_function = NULL;
    }
}

bool
ipa_struct_reorg::rewrite_lhs_rhs (tree lhs, tree rhs,
				   tree newlhs[max_split],
				   tree newrhs[max_split])
{
  bool l = rewrite_expr (lhs, newlhs);
  bool r = rewrite_expr (rhs, newrhs);

  /* Handle NULL pointer specially.  */
  if (l && !r && integer_zerop (rhs))
    {
      r = true;
      for (unsigned i = 0; i < max_split && newlhs[i]; i++)
	newrhs[i] = fold_convert (TREE_TYPE (newlhs[i]), rhs);
    }

  return l || r;
}

bool
ipa_struct_reorg::rewrite_expr (tree expr,
				tree newexpr[max_split],
				bool ignore_missing_decl)
{
  tree base;
  bool indirect;
  srtype *t;
  srfield *f;
  bool realpart, imagpart;
  bool address;
  bool escape_from_base = false;

  tree newbase[max_split];
  memset (newbase, 0, sizeof (tree[max_split]));
  memset (newexpr, 0, sizeof (tree[max_split]));

  if (TREE_CODE (expr) == CONSTRUCTOR)
    {
      srtype *t = find_type (TREE_TYPE (expr));
      if (!t)
	return false;
      gcc_assert (CONSTRUCTOR_NELTS (expr) == 0);
      if (!t->has_new_type ())
	return false;
      for (unsigned i = 0; i < max_split && t->newtype[i]; i++)
	newexpr[i] = build_constructor (t->newtype[i], NULL);
      return true;
    }

  if (!get_type_field (expr, base, indirect, t, f, realpart, imagpart,
		       address, escape_from_base))
    return false;

  /* If the type is not changed, then just return false.  */
  if (!t->has_new_type ())
    return false;

  /*  NULL pointer handling is "special".  */
  if (integer_zerop (base))
    {
      gcc_assert (indirect && !address);
      for (unsigned i = 0; i < max_split && t->newtype[i]; i++)
	{
	  tree newtype1 = reconstruct_complex_type (TREE_TYPE (base),
						    t->newtype[i]);
	  newbase[i] = fold_convert (newtype1, base);
	}
    }
  else
    {
      srdecl *d = find_decl (base);

      if (!d && dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Can't find decl:\n");
	  print_generic_expr (dump_file, base);
	  fprintf (dump_file, "\ntype:\n");
	  t->dump (dump_file);
	}
      if (!d && ignore_missing_decl)
	return true;
      gcc_assert (d);
      memcpy (newbase, d->newdecl, sizeof (d->newdecl));
    }

  if (f == NULL)
    {
      memcpy (newexpr, newbase, sizeof (newbase));
      for (unsigned i = 0; i < max_split && newexpr[i]; i++)
	{
	  if (address)
	    newexpr[i] = build_fold_addr_expr (newexpr[i]);
	  if (indirect)
	    newexpr[i] = build_simple_mem_ref (newexpr[i]);
	  if (imagpart)
	    newexpr[i] = build1 (IMAGPART_EXPR,
				 TREE_TYPE (TREE_TYPE (newexpr[i])),
				 newexpr[i]);
	  if (realpart)
	    newexpr[i] = build1 (REALPART_EXPR,
				 TREE_TYPE (TREE_TYPE (newexpr[i])),
				 newexpr[i]);
	}
      return true;
    }
  cur_srfd = f;

  tree newdecl = newbase[f->clusternum];
  for (unsigned i = 0; i < max_split && f->newfield[i]; i++)
    {
      tree newbase1 = newdecl;
      if (address)
	newbase1 = build_fold_addr_expr (newbase1);
      if (indirect)
	{
	  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	    {
	      /* Supports the MEM_REF offset.
		 _1 = MEM[(struct arc *)ap_1 + 72B].flow;
		 Old rewrite: _1 = ap.reorder.0_8->flow;
		 New rewrite: _1
		  = MEM[(struct arc.reorder.0 *)ap.reorder.0_8 + 64B].flow;
	      */
	      HOST_WIDE_INT offset_tmp = 0;
	      HOST_WIDE_INT mem_offset = 0;
	      bool realpart_tmp = false;
	      bool imagpart_tmp = false;
	      tree accesstype_tmp = NULL_TREE;
	      tree num = NULL_TREE;
	      get_ref_base_and_offset (expr, offset_tmp,
				       realpart_tmp, imagpart_tmp,
				       accesstype_tmp, &num);

	      tree ptype = TREE_TYPE (newbase1);
	      /* Specify the correct size for the multi-layer pointer.  */
	      tree size = isptrptr (ptype) ? TYPE_SIZE_UNIT (ptype) :
			  TYPE_SIZE_UNIT (inner_type (ptype));
	      mem_offset = (num != NULL)
			     ? TREE_INT_CST_LOW (num) * tree_to_shwi (size)
			     : 0;
	      newbase1 = build2 (MEM_REF, TREE_TYPE (ptype), newbase1,
				 build_int_cst (ptype, mem_offset));
	    }
	  else
	    newbase1 = build_simple_mem_ref (newbase1);
	}
      newexpr[i] = build3 (COMPONENT_REF, TREE_TYPE (f->newfield[i]),
			   newbase1, f->newfield[i], NULL_TREE);
      if (imagpart)
	newexpr[i] = build1 (IMAGPART_EXPR,
			     TREE_TYPE (TREE_TYPE (newexpr[i])),
			     newexpr[i]);
      if (realpart)
	newexpr[i] = build1 (REALPART_EXPR,
			     TREE_TYPE (TREE_TYPE (newexpr[i])),
			     newexpr[i]);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "cluster: %d. decl = ", (int)f->clusternum);
	  print_generic_expr (dump_file, newbase1);
	  fprintf (dump_file, "\nnewexpr = ");
	  print_generic_expr (dump_file, newexpr[i]);
	  fprintf (dump_file, "\n");
	}
    }
  return true;
}

/* Emit a series of gimples to compress the pointer to the index relative to
   the global header.  The basic blocks where gsi is located must have at least
   one stmt.  */

tree
ipa_struct_reorg::compress_ptr_to_offset (tree xhs, srtype *type,
					  gimple_stmt_iterator *gsi)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nCompress candidate pointer:\n");
      print_generic_expr (dump_file, xhs);
      fprintf (dump_file, "\nto offset:\n");
    }

  /* Emit gimple _X1 = ptr - gptr.  */
  tree pointer_addr = fold_convert (long_unsigned_type_node, xhs);
  tree gptr_addr = fold_convert (long_unsigned_type_node, type->pc_gptr);
  tree step1 = gimplify_build2 (gsi, MINUS_EXPR, long_unsigned_type_node,
				pointer_addr, gptr_addr);

  /* Emit gimple _X2 = _X1 / sizeof (struct).  */
  tree step2 = gimplify_build2 (gsi, TRUNC_DIV_EXPR, long_unsigned_type_node,
				step1, TYPE_SIZE_UNIT (type->newtype[0]));

  /* Emit _X3 = (compressed_size) _X2.  */
  tree pc_type = make_unsigned_type (compressed_size);
  tree step3 = gimplify_build1 (gsi, NOP_EXPR, pc_type, step2);

  /* Emit gimple _X4 = _X3 + 1.  */
  tree step4 = gimplify_build2 (gsi, PLUS_EXPR, pc_type, step3,
				build_one_cst (pc_type));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_generic_expr (dump_file, step4);
      fprintf (dump_file, "\n");
    }
  return step4;
}

/* Emit a series of gimples to decompress the index into the original
   pointer.  The basic blocks where gsi is located must have at least
   one stmt.  */

tree
ipa_struct_reorg::decompress_offset_to_ptr (tree xhs, srtype *type,
					    gimple_stmt_iterator *gsi)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nDecompress candidate offset:\n");
      print_generic_expr (dump_file, xhs);
      fprintf (dump_file, "\nto pointer:\n");
    }

  /* Emit _X1 = xhs - 1.  */
  tree offset = fold_convert (long_unsigned_type_node, xhs);
  tree step1 = gimplify_build2 (gsi, MINUS_EXPR, long_unsigned_type_node,
				offset,
				build_one_cst (long_unsigned_type_node));

  /* Emit _X2 = _X1 * sizeof (struct).  */
  tree step2 = gimplify_build2 (gsi, MULT_EXPR, long_unsigned_type_node,
				step1, TYPE_SIZE_UNIT (type->newtype[0]));

  /* Emit _X3 = phead + _X2.  */
  tree gptr_addr = fold_convert (long_unsigned_type_node, type->pc_gptr);
  tree step3 = gimplify_build2 (gsi, PLUS_EXPR, long_unsigned_type_node,
				gptr_addr, step2);

  /* Emit _X4 = (struct *) _X3.  */
  tree step4 = gimplify_build1 (gsi, NOP_EXPR, TREE_TYPE (type->pc_gptr),
				step3);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_generic_expr (dump_file, step4);
      fprintf (dump_file, "\n");
    }
  return step4;
}

/* Return the compression candidate srtype of SSA_NAME or COMPONENT_REF.  */

srtype *
ipa_struct_reorg::get_compression_candidate_type (tree xhs)
{
  if (xhs == NULL_TREE)
    return NULL;

  if (TREE_CODE (xhs) == SSA_NAME || TREE_CODE (xhs) == COMPONENT_REF)
    {
      srtype *access_type = find_type (inner_type (TREE_TYPE (xhs)));
      if (access_type != NULL && access_type->pc_candidate)
	return access_type;
    }
  return NULL;
}

/* True if the input type is the candidate type for pointer compression.  */

bool
ipa_struct_reorg::pc_candidate_st_type_p (tree type)
{
  if (type == NULL_TREE)
    return false;

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE)
	{
	  srtype *access_type = find_type (TREE_TYPE (type));
	  if (access_type != NULL && access_type->pc_candidate)
	    return true;
	}
    }
  return false;
}

/* True if the input xhs is a candidate for pointer compression.  */

bool
ipa_struct_reorg::pc_candidate_tree_p (tree xhs)
{
  if (xhs == NULL_TREE)
    return false;

  if (TREE_CODE (xhs) == COMPONENT_REF)
    {
      srtype *base_type = find_type (TREE_TYPE (TREE_OPERAND (xhs, 0)));
      if (base_type == NULL || base_type->has_escaped ())
	return false;

      return pc_candidate_st_type_p (TREE_TYPE (xhs));
    }
  return false;
}

srtype *
ipa_struct_reorg::get_semi_relayout_candidate_type (tree xhs)
{
  if (xhs == NULL)
    return NULL;
  if (TREE_CODE (xhs) == SSA_NAME || TREE_CODE (xhs) == COMPONENT_REF)
    {
      srtype *access_type = find_type (inner_type (TREE_TYPE (xhs)));
      if (access_type != NULL && access_type->semi_relayout)
	return access_type;
    }
  return NULL;
}

bool
ipa_struct_reorg::is_semi_relayout_candidate (tree xhs)
{
  if (xhs == NULL)
    return false;

  if (TREE_CODE (xhs) == SSA_NAME)
    xhs = TREE_TYPE (xhs);

  if (TREE_CODE (xhs) == POINTER_TYPE)
    {
      srtype *var_type = find_type (TREE_TYPE (xhs));
      if (!var_type || var_type->has_escaped ())
	return false;
      if (var_type->semi_relayout)
	return true;
    }

  if (TREE_CODE (xhs) == COMPONENT_REF)
    {
      tree mem = TREE_OPERAND (xhs, 0);
      if (TREE_CODE (mem) == MEM_REF)
	{
	  tree type = TREE_TYPE (mem);
	  srtype *old_type = get_relayout_candidate_type (type);
	  if (!old_type)
	    return false;
	  if (types_compatible_p (type, old_type->type)
	      && old_type->semi_relayout)
	    return true;
  	}
    }
  return false;
}

/* True if xhs is a component_ref that base has escaped but uses a compression
   candidate type.  */

bool
ipa_struct_reorg::pc_type_conversion_candidate_p (tree xhs)
{
  if (xhs == NULL_TREE)
    return false;

  if (TREE_CODE (xhs) == COMPONENT_REF)
    {
      tree mem = TREE_OPERAND (xhs, 0);
      if (TREE_CODE (mem) != MEM_REF)
	return false;
      srtype *base_type = find_type (TREE_TYPE (mem));
      if (base_type != NULL && base_type->has_escaped ())
	return pc_candidate_st_type_p (TREE_TYPE (xhs));

    }
  return false;
}

/* Creates a new basic block with zero for compressed null pointers.  */

basic_block
ipa_struct_reorg::create_bb_for_compress_nullptr (basic_block last_bb,
						  tree &phi)
{
  basic_block new_bb = create_empty_bb (last_bb);
  if (last_bb->loop_father != NULL)
    {
      add_bb_to_loop (new_bb, last_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  /* Emit phi = 0.  */
  gimple_stmt_iterator gsi = gsi_last_bb (new_bb);
  phi = make_ssa_name (make_unsigned_type (compressed_size));
  tree rhs = build_int_cst (make_unsigned_type (compressed_size), 0);
  gimple *new_stmt = gimple_build_assign (phi, rhs);
  gsi_insert_after (&gsi, new_stmt, GSI_NEW_STMT);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nCreate bb %d for compress nullptr:\n",
	       new_bb->index);
      gimple_dump_bb (dump_file, new_bb, 0, dump_flags);
    }
  return new_bb;
}

/* Create a new basic block to compress the pointer to the index relative to
   the allocated memory pool header.  */

basic_block
ipa_struct_reorg::create_bb_for_compress_candidate (basic_block last_bb,
						    tree new_rhs, srtype *type,
						    tree &phi)
{
  basic_block new_bb = create_empty_bb (last_bb);
  if (last_bb->loop_father != NULL)
    {
      add_bb_to_loop (new_bb, last_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  gimple_stmt_iterator gsi = gsi_last_bb (new_bb);
  /* compress_ptr_to_offset () needs at least one stmt in target bb.  */
  gsi_insert_after (&gsi, gimple_build_nop (), GSI_NEW_STMT);
  phi = compress_ptr_to_offset (new_rhs, type, &gsi);
  /* Remove the NOP created above.  */
  gsi_remove (&gsi, true);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nCreate bb %d for compress candidate:\n",
	       new_bb->index);
      gimple_dump_bb (dump_file, new_bb, 0, dump_flags);
    }
  return new_bb;
}

/* Compression can be simplified by these following cases:
     1.  if rhs is NULL, uses zero to represent it.
     2.  if new_rhs has been converted into INTEGER_TYPE in the previous stmt,
	 just use it here.  For example:
	    _1 = t->s
	 -> tt->s = _1.  */

bool
ipa_struct_reorg::pc_direct_rewrite_chance_p (tree rhs, tree &new_rhs)
{
  if (integer_zerop (rhs))
    {
      new_rhs = build_int_cst (make_unsigned_type (compressed_size), 0);
      return true;
    }
  else if (new_rhs && TREE_CODE (TREE_TYPE (new_rhs)) == INTEGER_TYPE)
    {
      return true;
    }
  return false;
}

/* The following cases can simplify the checking of null pointer:
     1. rhs defined from POINTER_PLUS_EXPR.
     2. rhs used as COMPONENT_REF in this basic block.  */

bool
ipa_struct_reorg::pc_simplify_chance_for_compress_p (gassign *stmt,
						     tree rhs)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);

  if (def_stmt && is_gimple_assign (def_stmt)
      && gimple_assign_rhs_code (def_stmt) == POINTER_PLUS_EXPR)
    return true;

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, rhs)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (use_stmt->bb != stmt->bb || !is_gimple_assign (use_stmt))
	continue;

      tree use_rhs = gimple_assign_rhs1 (use_stmt);
      if (TREE_CODE (use_rhs) == COMPONENT_REF
	  && TREE_OPERAND (TREE_OPERAND (use_rhs, 0), 0) == rhs)
	return true;
    }
  return false;
}

/* Perform compression directly without checking null pointer.  */

bool
ipa_struct_reorg::compress_candidate_without_check (gimple_stmt_iterator *gsi,
						    tree rhs,
						    tree &new_rhs)
{
  srtype *type = get_compression_candidate_type (rhs);
  gcc_assert (type != NULL);
  new_rhs = compress_ptr_to_offset (new_rhs, type, gsi);
  return true;
}

/* Perform pointer compression with check.  The conversion will be as shown in
   the following example:
     Orig bb:
     bb <1>:
     _1->t = _2

     will be transformed to:
     bb <1>:
     _3 = _2
     if (_2 == NULL)
       goto bb <2>
     else
       goto bb <3>

     bb <2>:
     _3 = 0
     goto bb <4>

     bb <3>:
     ...
     _4 = compress (_2)
     goto bb <4>

     bb <4>:
     _5 = PHI (_3, _4)
     _1->t = _5
   The gsi will move to the beginning of split dst bb <4>, _1->t = _5 will be
   emitted by rewrite_assign ().  */

bool
ipa_struct_reorg::compress_candidate_with_check (gimple_stmt_iterator *gsi,
						 tree rhs, tree &new_rhs)
{
  tree cond_lhs = make_ssa_name (TREE_TYPE (new_rhs));
  gimple *assign_stmt = gimple_build_assign (cond_lhs, new_rhs);
  gsi_insert_before (gsi, assign_stmt, GSI_SAME_STMT);

  /* Insert cond stmt.  */
  tree rhs_pointer_type = build_pointer_type (TREE_TYPE (new_rhs));
  gcond *cond = gimple_build_cond (EQ_EXPR, cond_lhs,
				   build_int_cst (rhs_pointer_type, 0),
				   NULL_TREE, NULL_TREE);
  gimple_set_location (cond, UNKNOWN_LOCATION);
  gsi_insert_before (gsi, cond, GSI_SAME_STMT);

  edge e = split_block (cond->bb, cond);
  basic_block split_src_bb = e->src;
  basic_block split_dst_bb = e->dest;

  /* Create bb for nullptr.  */
  tree phi1 = NULL_TREE;
  basic_block true_bb = create_bb_for_compress_nullptr (split_src_bb, phi1);

  /* Create bb for comprssion.  */
  srtype *type = get_compression_candidate_type (rhs);
  gcc_assert (type != NULL);
  tree phi2 = NULL_TREE;
  basic_block false_bb = create_bb_for_compress_candidate (true_bb, new_rhs,
							   type, phi2);

  /* Rebuild and reset cfg.  */
  remove_edge_raw (e);

  edge etrue = make_edge (split_src_bb, true_bb, EDGE_TRUE_VALUE);
  etrue->probability = profile_probability::unlikely ();
  true_bb->count = etrue->count ();

  edge efalse = make_edge (split_src_bb, false_bb, EDGE_FALSE_VALUE);
  efalse->probability = profile_probability::likely ();
  false_bb->count = efalse->count ();

  edge e1 = make_single_succ_edge (true_bb, split_dst_bb, EDGE_FALLTHRU);
  edge e2 = make_single_succ_edge (false_bb, split_dst_bb, EDGE_FALLTHRU);

  tree phi = make_ssa_name (make_unsigned_type (compressed_size));
  gphi *phi_node = create_phi_node (phi, split_dst_bb);
  add_phi_arg (phi_node, phi1, e1, UNKNOWN_LOCATION);
  add_phi_arg (phi_node, phi2, e2, UNKNOWN_LOCATION);

  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, split_dst_bb, split_src_bb);
      set_immediate_dominator (CDI_DOMINATORS, true_bb, split_src_bb);
      set_immediate_dominator (CDI_DOMINATORS, false_bb, split_src_bb);
    }
  *gsi = gsi_start_bb (split_dst_bb);
  new_rhs = phi;
  return true;
}

/* If there is a direct rewrite chance or simplification opportunity, perform
   the simplified compression rewrite.  Otherwise, create a cond expression and
   two basic blocks to implement pointer compression.  */

bool
ipa_struct_reorg::compress_candidate (gassign *stmt, gimple_stmt_iterator *gsi,
				      tree rhs, tree &new_rhs)
{
  if (pc_direct_rewrite_chance_p (rhs, new_rhs))
    return true;
  else if (current_layout_opt_level & POINTER_COMPRESSION_UNSAFE
	   && pc_simplify_chance_for_compress_p (stmt, rhs))
    return compress_candidate_without_check (gsi, rhs, new_rhs);

  return compress_candidate_with_check (gsi, rhs, new_rhs);
}

/* Create a new basic block to decompress the index to null pointer.  */

basic_block
ipa_struct_reorg::create_bb_for_decompress_nullptr (basic_block last_bb,
						    tree new_rhs,
						    tree &phi_node)
{
  basic_block new_bb = create_empty_bb (last_bb);
  if (last_bb->loop_father != NULL)
    {
      add_bb_to_loop (new_bb, last_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }
  gimple_stmt_iterator gsi = gsi_last_bb (new_bb);
  tree rhs_pointer_type = build_pointer_type (TREE_TYPE (new_rhs));
  phi_node = make_ssa_name (rhs_pointer_type);
  gimple *new_stmt = gimple_build_assign (phi_node,
					  build_int_cst (rhs_pointer_type, 0));
  gsi_insert_after (&gsi, new_stmt, GSI_NEW_STMT);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nCreate bb %d for decompress nullptr:\n",
	       new_bb->index);
      gimple_dump_bb (dump_file, new_bb, 0, dump_flags);
    }
  return new_bb;
}

/* Create a new basic block to decompress the index into original pointer.  */

basic_block
ipa_struct_reorg::create_bb_for_decompress_candidate (basic_block last_bb,
						      tree lhs, srtype *type,
						      tree &phi_node)
{
  basic_block new_bb = create_empty_bb (last_bb);
  if (last_bb->loop_father != NULL)
    {
      add_bb_to_loop (new_bb, last_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }
  gimple_stmt_iterator gsi = gsi_last_bb (new_bb);
  /* decompress_ptr_to_offset () needs at least one stmt in target bb.  */
  gsi_insert_after (&gsi, gimple_build_nop (), GSI_NEW_STMT);
  phi_node = decompress_offset_to_ptr (lhs, type, &gsi);
  /* Remove the NOP created above.  */
  gsi_remove (&gsi, true);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nCreate bb %d for decompress candidate:\n",
	       new_bb->index);
      gimple_dump_bb (dump_file, new_bb, 0, dump_flags);
    }
  return new_bb;
}

/* Try decompress candidate without check.  */

bool
ipa_struct_reorg::decompress_candidate_without_check (gimple_stmt_iterator *gsi,
						      tree lhs, tree rhs,
						      tree &new_lhs,
						      tree &new_rhs)
{
  bool processed = false;

  if (!gsi_one_before_end_p (*gsi))
    {
      gsi_next (gsi);
      gimple *next_stmt = gsi_stmt (*gsi);
      if (gimple_code (next_stmt) == GIMPLE_ASSIGN
	  && gimple_assign_rhs_class (next_stmt) == GIMPLE_SINGLE_RHS)
	{
	  tree next_rhs = gimple_assign_rhs1 (next_stmt);
	  /* If current lhs is used as rhs in the next stmt:
	     -> _1 = t->s
		tt->s = _1.  */
	  if (lhs == next_rhs)
	    {
	      /* Check whether:
	       1. the lhs is only used in the next stmt.
	       2. the next lhs is candidate type.  */
	      if (has_single_use (lhs)
		  && pc_candidate_tree_p (gimple_assign_lhs (next_stmt)))
		{
		  processed = true;
		  /* Copy directly without conversion after update type.  */
		  TREE_TYPE (new_lhs)
		    = make_unsigned_type (compressed_size);
		}
	    }
	  /* -> _1 = t->s
		_2 = _1->s
	     In this case, _1 might not be nullptr, so decompress it without
	     check.  */
	  else if (TREE_CODE (next_rhs) == COMPONENT_REF)
	    {
	      tree use_base = TREE_OPERAND (TREE_OPERAND (next_rhs, 0), 0);
	      if (use_base == lhs)
		{
		  srtype *type = get_compression_candidate_type (rhs);
		  gcc_assert (type != NULL);
		  gsi_prev (gsi);
		  tree new_ref = NULL_TREE;
		  if (TREE_CODE (new_rhs) == MEM_REF)
		    new_ref = new_rhs;
		  else
		    {
		      tree base = TREE_OPERAND (TREE_OPERAND (new_rhs, 0), 0);
		      tree new_mem_ref = build_simple_mem_ref (base);
		      new_ref = build3 (COMPONENT_REF,
					TREE_TYPE (new_rhs),
					new_mem_ref,
					TREE_OPERAND (new_rhs, 1),
					NULL_TREE);
		    }
		  new_rhs = decompress_offset_to_ptr (new_ref, type, gsi);
		  processed = true;
		  gsi_next (gsi);
		}
	    }
	}
      gsi_prev (gsi);
      return processed;
    }
  return false;
}

/* Perform pointer decompression with check.  The conversion will be as shown
   in the following example:
     Orig bb:
     bb <1>:
     _1 = _2->t

     will be transformed to:
     bb <1>:
     _3 = _2->t
     if (_3 == 0)
       goto bb <2>
     else
       goto bb <3>

     bb <2>:
     _4 = NULL
     goto bb <4>

     bb <3>:
     ...
     _5 = decompress (_3)
     goto bb <4>

     bb <4>:
     _6 = PHI (_4, _5)
     _1 = _6
   The gsi will move to the beginning of split dst bb <4>, _1 = _6 will be
   emitted by rewrite_assign ().  */

bool
ipa_struct_reorg::decompress_candidate_with_check (gimple_stmt_iterator *gsi,
						   tree rhs, tree &new_rhs)
{
  /* Insert cond stmt.  */
  tree cond_lhs = make_ssa_name (TREE_TYPE (new_rhs));
  gassign *cond_assign = gimple_build_assign (cond_lhs, new_rhs);
  gsi_insert_before (gsi, cond_assign, GSI_SAME_STMT);

  tree pc_type = make_unsigned_type (compressed_size);
  gcond *cond = gimple_build_cond (EQ_EXPR, cond_lhs,
				   build_int_cst (pc_type, 0),
				   NULL_TREE, NULL_TREE);
  gimple_set_location (cond, UNKNOWN_LOCATION);
  gsi_insert_before (gsi, cond, GSI_SAME_STMT);

  /* Split bb.  */
  edge e = split_block (cond->bb, cond);
  basic_block split_src_bb = e->src;
  basic_block split_dst_bb = e->dest;

  /* Create bb for decompress nullptr.  */
  tree phi1 = NULL_TREE;
  basic_block true_bb = create_bb_for_decompress_nullptr (split_src_bb,
							  new_rhs, phi1);

  /* Create bb for decomprssion candidate.  */
  tree phi2 = NULL_TREE;
  srtype *type = get_compression_candidate_type (rhs);
  gcc_assert (type != NULL);
  basic_block false_bb = create_bb_for_decompress_candidate (true_bb, cond_lhs,
							     type, phi2);

  /* Refresh and reset cfg.  */
  remove_edge_raw (e);

  edge etrue = make_edge (split_src_bb, true_bb, EDGE_TRUE_VALUE);
  etrue->probability = profile_probability::unlikely ();
  true_bb->count = etrue->count ();

  edge efalse = make_edge (split_src_bb, false_bb, EDGE_FALSE_VALUE);
  efalse->probability = profile_probability::likely ();
  false_bb->count = efalse->count ();

  edge e1 = make_single_succ_edge (true_bb, split_dst_bb, EDGE_FALLTHRU);
  edge e2 = make_single_succ_edge (false_bb, split_dst_bb, EDGE_FALLTHRU);

  tree phi = make_ssa_name (build_pointer_type (TREE_TYPE (cond_lhs)));
  gphi *phi_node = create_phi_node (phi, split_dst_bb);
  add_phi_arg (phi_node, phi1, e1, UNKNOWN_LOCATION);
  add_phi_arg (phi_node, phi2, e2, UNKNOWN_LOCATION);

  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, split_dst_bb, split_src_bb);
      set_immediate_dominator (CDI_DOMINATORS, true_bb, split_src_bb);
      set_immediate_dominator (CDI_DOMINATORS, false_bb, split_src_bb);
    }
  *gsi = gsi_start_bb (split_dst_bb);
  new_rhs = phi;
  return true;
}

/* If there is a simplification opportunity, perform the simplified
   decompression rewrite.  Otherwise, create a cond expression and two basic
   blocks to implement pointer decompression.  */

bool
ipa_struct_reorg::decompress_candidate (gimple_stmt_iterator *gsi,
					tree lhs, tree rhs, tree &new_lhs,
					tree &new_rhs)
{
  if (current_layout_opt_level & POINTER_COMPRESSION_UNSAFE
      && decompress_candidate_without_check (gsi, lhs, rhs, new_lhs, new_rhs))
    return true;

  return decompress_candidate_with_check (gsi, rhs, new_rhs);
}

/* Try to perform pointer compression and decompression.  */

void
ipa_struct_reorg::try_rewrite_with_pointer_compression (gassign *stmt,
							gimple_stmt_iterator
							*gsi, tree lhs,
							tree rhs, tree &new_lhs,
							tree &new_rhs)
{
  bool l = pc_candidate_tree_p (lhs);
  bool r = pc_candidate_tree_p (rhs);
  if (!l && !r)
    {
      tree tmp_rhs = new_rhs == NULL_TREE ? rhs : new_rhs;
      if (pc_type_conversion_candidate_p (lhs))
	{
	  /* Transfer MEM[(struct *)_1].files = _4;
	     to _tmp = (struct *)_4;
		MEM[(struct *)_1].files = _tmp; */
	  tree tmp_reg = create_tmp_reg (TREE_TYPE (lhs));
	  tree tmp_rhs_cvt = fold_convert (TREE_TYPE (lhs), tmp_rhs);
	  gimple *copy_stmt = gimple_build_assign (tmp_reg, tmp_rhs_cvt);
	  gsi_insert_before (gsi, copy_stmt, GSI_SAME_STMT);
	  new_rhs = tmp_reg;
	}
      else if (pc_type_conversion_candidate_p (rhs))
	{
	  /* Transfer _4 = MEM[(struct *)_1].nodes;
	     to _tmp = MEM[(struct *)_1].nodes;
		_4  = (new_struct *) _tmp; */
	  tree tmp_reg = create_tmp_reg (TREE_TYPE (new_lhs));
	  gimple *copy_stmt = gimple_build_assign (tmp_reg, tmp_rhs);
	  gsi_insert_before (gsi, copy_stmt, GSI_SAME_STMT);
	  new_rhs = fold_convert (TREE_TYPE (new_lhs), tmp_reg);
	}
    }
  else if (l && r)
    gcc_unreachable ();
  else if (l)
    {
      if (!compress_candidate (stmt, gsi, rhs, new_rhs))
	gcc_unreachable ();
    }
  else if (r)
    {
      if (!decompress_candidate (gsi, lhs, rhs, new_lhs, new_rhs))
	gcc_unreachable ();
    }
}

tree
ipa_struct_reorg::rewrite_pointer_diff (gimple_stmt_iterator *gsi, tree ptr1,
					tree ptr2, srtype *type)
{
  tree shifts = build_int_cst (long_integer_type_node, semi_relayout_align);
  tree pointer_type = build_pointer_type (unsigned_char_type_node);
  // tree pointer_type = build_pointer_type (long_integer_type_node);
  tree intptr_type = signed_type_for (pointer_type);

  /* addr_high_1 = (intptr_t)ptr1 >> shifts  */
  tree ptr1_cvt = fold_convert (intptr_type, ptr1);
  tree addr_high_1 = gimplify_build2 (gsi, RSHIFT_EXPR, intptr_type,
				      ptr1_cvt, shifts);
  /* addr_high_2 = (intptr_t)ptr2 >> shifts  */
  tree ptr2_cvt = fold_convert (intptr_type, ptr2);
  tree addr_high_2 = gimplify_build2 (gsi, RSHIFT_EXPR, intptr_type,
				      ptr2_cvt, shifts);
  /* off1 = (intptr_t)ptr1 - (addr_high_1 << shifts)  */
  tree bucket_start_1 = gimplify_build2 (gsi, LSHIFT_EXPR, intptr_type,
					 addr_high_1, shifts);
  tree off1 = gimplify_build2 (gsi, MINUS_EXPR, intptr_type,
			       ptr1_cvt, bucket_start_1);
  /* off2 = (intptr_t)ptr2 - (addr_high_2 << shifts)  */
  tree bucket_start_2 = gimplify_build2 (gsi, LSHIFT_EXPR, intptr_type,
					 addr_high_2, shifts);
  tree off2 = gimplify_build2 (gsi, MINUS_EXPR, intptr_type,
			       ptr2_cvt, bucket_start_2);
  /* group_diff = (addr_high_1 - addr_high_2) / bucket_parts  */
  tree bucket_sub = gimplify_build2 (gsi, MINUS_EXPR, intptr_type,
				     addr_high_1, addr_high_2);
  tree bucket_parts = build_int_cst (intptr_type,
				     type->bucket_parts);
  tree group_diff = gimplify_build2 (gsi, TRUNC_DIV_EXPR,
				     intptr_type,
				     bucket_sub, bucket_parts);
  /* off_addr_diff = off1 - off2  */
  tree off_addr_diff = gimplify_build2 (gsi, MINUS_EXPR, intptr_type,
					off1, off2);
  /* res = group_diff * bucket_capacity + off_diff / 8  */
  tree capacity = build_int_cst (long_integer_type_node,
				 relayout_part_size / 8);
  tree unit_size = build_int_cst (long_integer_type_node, 8);
  tree bucket_index_diff = gimplify_build2 (gsi, MULT_EXPR,
					    intptr_type,
					    group_diff, capacity);
  tree off_index = gimplify_build2 (gsi, TRUNC_DIV_EXPR,
				    long_integer_type_node,
				    off_addr_diff, unit_size);
  tree res = gimplify_build2 (gsi, PLUS_EXPR, intptr_type,
			      bucket_index_diff, off_index);
  return res;
}

basic_block
create_bb_for_group_diff_eq_0 (basic_block last_bb, tree phi, tree new_granule)
{
  basic_block new_bb = create_empty_bb (last_bb);
  if (last_bb->loop_father != NULL)
    {
      add_bb_to_loop (new_bb, last_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }
  /* Emit res = new_granule;  */
  gimple_stmt_iterator gsi = gsi_last_bb (new_bb);
  gimple *new_stmt = gimple_build_assign (phi, new_granule);
  gsi_insert_after (&gsi, new_stmt, GSI_NEW_STMT);
  return new_bb;
}

basic_block
create_bb_for_group_diff_ne_0 (basic_block new_bb, tree &phi, tree ptr,
			       tree group_diff, tree off_times_8, srtype *type)
{
  tree intptr_type = signed_type_for (long_unsigned_type_node);
  tree shifts = build_int_cst (intptr_type, semi_relayout_align);
  gimple_stmt_iterator gsi = gsi_last_bb (new_bb);
  gsi_insert_after (&gsi, gimple_build_nop (), GSI_NEW_STMT);
  tree ptr_cvt = fold_convert (intptr_type, ptr);
  /* curr_group_start = (ptr >> shifts) << shifts;  */
  tree ptr_r_1 = gimplify_build2 (&gsi, RSHIFT_EXPR, intptr_type,
				  ptr_cvt, shifts);
  tree curr_group_start = gimplify_build2 (&gsi, LSHIFT_EXPR, intptr_type,
					   ptr_r_1, shifts);
  /* curr_off_from_group = ptr - curr_group_start;  */
  tree curr_off_from_group = gimplify_build2 (&gsi, MINUS_EXPR,
					      intptr_type,
					      ptr_cvt, curr_group_start);
  /* res = curr_group_start + ((group_diff * parts) << shifts)
	   + ((curr_off_from_group + off_times_8) % shifts);  */
  tree step1 = gimplify_build2 (&gsi, MULT_EXPR, long_integer_type_node,
				group_diff, build_int_cst (
				long_integer_type_node, type->bucket_parts));
  tree step1_cvt = fold_convert (intptr_type, step1);
  tree step2 = gimplify_build2 (&gsi, LSHIFT_EXPR, intptr_type,
				step1_cvt, shifts);
  tree off_times_8_cvt = fold_convert (intptr_type, off_times_8);
  tree step3 = gimplify_build2 (&gsi, PLUS_EXPR, intptr_type,
				curr_off_from_group, off_times_8_cvt);
  tree step4 = gimplify_build2 (&gsi, TRUNC_MOD_EXPR, intptr_type,
				step3, build_int_cst (intptr_type,
				relayout_part_size));
  tree step5 = gimplify_build2 (&gsi, PLUS_EXPR, intptr_type,
				step2, step4);
  tree res_phi1 = gimplify_build2 (&gsi, PLUS_EXPR, long_integer_type_node,
				   curr_group_start, step5);
  /* if (group_diff < 0)  */
  gcond *cond = gimple_build_cond (LT_EXPR, group_diff,
				   build_int_cst (long_integer_type_node, 0),
				   NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, cond, GSI_SAME_STMT);
  /* remove nop  */
  gsi_remove (&gsi, true);
  /* res += shifts  */
  basic_block true_bb = create_empty_bb (new_bb);
  if (new_bb->loop_father != NULL)
    {
      add_bb_to_loop (true_bb, new_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }
  gimple_stmt_iterator true_gsi = gsi_last_bb (true_bb);
  tree res_phi2 = make_ssa_name (long_integer_type_node);
  gimple *new_stmt
		= gimple_build_assign (res_phi2, PLUS_EXPR, res_phi1,
				       build_int_cst (long_integer_type_node,
				       relayout_part_size));
  gsi_insert_after (&true_gsi, new_stmt, GSI_NEW_STMT);
  /* create phi bb  */
  basic_block res_bb = create_empty_bb (true_bb);
  if (new_bb->loop_father != NULL)
    {
      add_bb_to_loop (res_bb, new_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }
  /* rebuild cfg  */
  edge etrue = make_edge (new_bb, true_bb, EDGE_TRUE_VALUE);
  etrue->probability = profile_probability::unlikely ();
  true_bb->count = etrue->count ();

  edge efalse = make_edge (new_bb, res_bb, EDGE_FALSE_VALUE);
  efalse->probability = profile_probability::likely ();
  res_bb->count = efalse->count ();

  edge efall = make_single_succ_edge (true_bb, res_bb, EDGE_FALLTHRU);

  phi = make_ssa_name (long_integer_type_node);
  gphi *phi_node = create_phi_node (phi, res_bb);
  add_phi_arg (phi_node, res_phi2, efall, UNKNOWN_LOCATION);
  add_phi_arg (phi_node, res_phi1, efalse, UNKNOWN_LOCATION);

  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, true_bb, new_bb);
      set_immediate_dominator (CDI_DOMINATORS, res_bb, new_bb);
    }
  return res_bb;
}

tree
ipa_struct_reorg::rewrite_pointer_plus_integer (gimple *stmt ATTRIBUTE_UNUSED,
						gimple_stmt_iterator *gsi,
						tree ptr, tree offset,
						srtype *type)
{
  gcc_assert (type->semi_relayout);
  tree off = fold_convert (long_integer_type_node, offset);
  tree num_8 = build_int_cst (long_integer_type_node, 8);
  tree shifts = build_int_cst (long_integer_type_node, semi_relayout_align);
  // tree shifts = build_int_cst (integer_type_node, semi_relayout_align);
  /* off_times_8 = off * 8;  */
  tree off_times_8 = gimplify_build2 (gsi, MULT_EXPR, long_integer_type_node,
				      off, num_8);
  /* new_granule = ptr + off * 8;  */
  tree ptr_int = fold_convert (long_integer_type_node, ptr);
  tree new_granule = gimplify_build2 (gsi, PLUS_EXPR, long_integer_type_node,
				      ptr_int, off_times_8);
  /* group_diff = (new_granule >> shifts) - (ptr >> shifts);  */
  tree group_diff_rhs_1 = gimplify_build2 (gsi, RSHIFT_EXPR,
					   long_integer_type_node,
					   new_granule, shifts);
  tree group_diff_rhs_2 = gimplify_build2 (gsi, RSHIFT_EXPR,
					   long_integer_type_node,
					   ptr_int, shifts);
  tree group_diff = gimplify_build2 (gsi, MINUS_EXPR, long_integer_type_node,
				     group_diff_rhs_1, group_diff_rhs_2);
  /* if (group_diff == 0)  */
  gcond *cond = gimple_build_cond (EQ_EXPR, group_diff,
				   build_int_cst (long_integer_type_node, 0),
				   NULL_TREE, NULL_TREE);
  gimple_set_location (cond, UNKNOWN_LOCATION);
  gsi_insert_before (gsi, cond, GSI_SAME_STMT);

  edge e = split_block (cond->bb, cond);
  basic_block split_src_bb = e->src;
  basic_block split_dst_bb = e->dest;
  remove_edge_raw (e);
  /* if (group_diff == 0)
       res = new_granule;  */
  tree res_phi_1 = make_ssa_name (long_integer_type_node);
  basic_block true_bb = create_bb_for_group_diff_eq_0 (split_src_bb, res_phi_1,
						       new_granule);
  /* else  */
  tree res_phi_2 = NULL_TREE;
  basic_block false_bb = create_empty_bb (split_src_bb);
  if (split_src_bb->loop_father != NULL)
    {
      add_bb_to_loop (false_bb, split_src_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  edge etrue = make_edge (split_src_bb, true_bb, EDGE_TRUE_VALUE);
  etrue->probability = profile_probability::very_likely ();
  true_bb->count = etrue->count ();

  edge efalse = make_edge (split_src_bb, false_bb, EDGE_FALSE_VALUE);
  efalse->probability = profile_probability::unlikely ();
  false_bb->count = efalse->count ();
  basic_block res_bb = create_bb_for_group_diff_ne_0 (false_bb, res_phi_2,
						      ptr_int, group_diff,
						      off_times_8, type);
  /* rebuild cfg  */
  edge e_true_fall = make_single_succ_edge (true_bb, split_dst_bb,
					    EDGE_FALLTHRU);
  edge e_false_fall = make_single_succ_edge (res_bb, split_dst_bb,
					     EDGE_FALLTHRU);
  tree res_int = make_ssa_name (long_integer_type_node);
  gphi *phi_node = create_phi_node (res_int, split_dst_bb);
  add_phi_arg (phi_node, res_phi_1, e_true_fall, UNKNOWN_LOCATION);
  add_phi_arg (phi_node, res_phi_2, e_false_fall, UNKNOWN_LOCATION);
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, split_dst_bb, split_src_bb);
      set_immediate_dominator (CDI_DOMINATORS, true_bb, split_src_bb);
      set_immediate_dominator (CDI_DOMINATORS, false_bb, split_src_bb);
    }
  *gsi = gsi_start_bb (split_dst_bb);
  tree pointer_type = build_pointer_type (unsigned_char_type_node);
  tree res = gimplify_build1 (gsi, NOP_EXPR, pointer_type, res_int);
  return res;
}

tree
ipa_struct_reorg::build_div_expr (gimple_stmt_iterator *gsi,
				  tree expr, tree orig_size)
{
  tree div_expr = build2 (TRUNC_DIV_EXPR, long_unsigned_type_node,
			  expr, orig_size);
  tree num = make_ssa_name (long_unsigned_type_node);
  gimple *g = gimple_build_assign (num, div_expr);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  return num;
}

srtype *
ipa_struct_reorg::get_relayout_candidate_type (tree type)
{
  if (type == NULL)
    return NULL;
  if (TREE_CODE (type) != RECORD_TYPE)
    return NULL;
  return find_type (inner_type (type));
}

long unsigned int
ipa_struct_reorg::get_true_field_offset (srfield *field, srtype *type)
{
  unsigned HOST_WIDE_INT new_offset;
  new_offset = *(type->new_field_offsets.get (field->newfield[0]));
  return new_offset;
}

tree
ipa_struct_reorg::get_true_pointer_base (gimple_stmt_iterator *gsi,
					 tree mem_ref, srtype *type)
{
  tree ptr = TREE_OPERAND (mem_ref, 0);
  tree off_bytes = TREE_OPERAND (mem_ref, 1);
  unsigned num = tree_to_shwi (off_bytes);
  if (num == 0)
    return ptr;
  tree orig_size = TYPE_SIZE_UNIT (TREE_TYPE (mem_ref));
  tree off = build_int_cst (long_integer_type_node,
			    num / tree_to_uhwi (orig_size));
  gimple *stmt = gsi_stmt (*gsi);
  tree new_pointer_base = rewrite_pointer_plus_integer (stmt, gsi, ptr,
							off, type);
  return new_pointer_base;
}

tree
ipa_struct_reorg::rewrite_address (tree pointer_base, srfield *field,
				   srtype *type, gimple_stmt_iterator *gsi)
{
  unsigned HOST_WIDE_INT field_offset = get_true_field_offset (field, type);

  tree pointer_ssa = fold_convert (long_unsigned_type_node, pointer_base);
  tree step1 = gimplify_build1 (gsi, NOP_EXPR, long_unsigned_type_node,
				pointer_ssa);
  tree new_offset_ssa = build_int_cst (long_unsigned_type_node, field_offset);
  tree step2 = gimplify_build2 (gsi, PLUS_EXPR, long_unsigned_type_node, step1,
				new_offset_ssa);
  tree field_ssa = fold_convert (
		   build_pointer_type (TREE_TYPE (field->newfield[0])), step2);
  tree step3 = gimplify_build1 (gsi, NOP_EXPR,
				TREE_TYPE (field_ssa), field_ssa);

  tree new_mem_ref = fold_build2 (MEM_REF, TREE_TYPE (field->newfield[0]),
				  step3, build_int_cst (
				  TREE_TYPE (field_ssa), 0));
  return new_mem_ref;
}

bool
ipa_struct_reorg::check_sr_copy (gimple *stmt)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (lhs) != MEM_REF || TREE_CODE (rhs) != MEM_REF)
    return false;
  srtype *t1 = get_relayout_candidate_type (TREE_TYPE (lhs));
  srtype *t2 = get_relayout_candidate_type (TREE_TYPE (rhs));
  if (!t1 || !t2 || !t1->semi_relayout || !t2->semi_relayout || t1 != t2)
    return false;
  tree pointer1 = TREE_OPERAND (lhs, 0);
  tree pointer2 = TREE_OPERAND (rhs, 0);
  if (TREE_CODE (TREE_TYPE (pointer1)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (pointer2)) != POINTER_TYPE)
    return false;

  tree type1 = TREE_TYPE (TREE_TYPE (pointer1));
  tree type2 = TREE_TYPE (TREE_TYPE (pointer2));

  srtype *t3 = get_relayout_candidate_type (type1);
  srtype *t4 = get_relayout_candidate_type (type2);

  if (t3 != t4 || t3 != t1)
    return false;

  return true;
}

void
ipa_struct_reorg::relayout_field_copy (gimple_stmt_iterator *gsi,
				       gimple *stmt ATTRIBUTE_UNUSED,
				       tree lhs, tree rhs ATTRIBUTE_UNUSED,
				       tree &newlhs, tree &newrhs)
{
  srtype *type = get_relayout_candidate_type (TREE_TYPE (lhs));
  tree lhs_base_pointer = get_true_pointer_base (gsi, newlhs, type);
  tree rhs_base_pointer = get_true_pointer_base (gsi, newrhs, type);
  tree new_l_mem_ref = NULL_TREE;
  tree new_r_mem_ref = NULL_TREE;
  srfield *field = NULL;
  unsigned i = 0;
  FOR_EACH_VEC_ELT (type->fields, i, field)
    {
      if (!field->newfield[0])
	continue;
      new_l_mem_ref = rewrite_address (lhs_base_pointer, field, type, gsi);
      new_r_mem_ref = rewrite_address (rhs_base_pointer, field, type, gsi);
      if (!is_gimple_reg (new_l_mem_ref))
	{
	  tree tmp_reg = create_tmp_reg (TREE_TYPE(new_l_mem_ref));
	  gimple *copy_stmt = gimple_build_assign (tmp_reg, new_r_mem_ref);
	  gsi_insert_before (gsi, copy_stmt, GSI_SAME_STMT);
	  new_r_mem_ref = tmp_reg;
	}
      gimple *new_stmt = gimple_build_assign (new_l_mem_ref, new_r_mem_ref);
      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
    }
  gcc_assert (new_l_mem_ref != NULL_TREE && new_r_mem_ref != NULL_TREE);
  newlhs = new_l_mem_ref;
  newrhs = new_r_mem_ref;
}

bool
ipa_struct_reorg::do_semi_relayout (gimple_stmt_iterator *gsi, gimple *stmt,
				    tree &newlhs, tree &newrhs)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);

  bool l = TREE_CODE (lhs) == COMPONENT_REF ? is_semi_relayout_candidate (lhs)
					    : false;
  bool r = TREE_CODE (rhs) == COMPONENT_REF ? is_semi_relayout_candidate (rhs)
					    : false;

  gcc_assert (!(l && r));

  if (!l && !r)
    {
      if (check_sr_copy (stmt))
	{
	  relayout_field_copy (gsi, stmt, lhs, rhs, newlhs, newrhs);
	  return true;
	}
    }
  else if (l)
    {
      srtype *type = get_relayout_candidate_type (
				TREE_TYPE (TREE_OPERAND (lhs, 0)));
      srfield *new_field = type->find_field (
				int_byte_position (TREE_OPERAND (lhs, 1)));
      tree pointer_base = get_true_pointer_base (
				gsi, TREE_OPERAND (newlhs, 0), type);
      newlhs = rewrite_address (pointer_base, new_field, type, gsi);
    }
  else if (r)
    {
      srtype *type = get_relayout_candidate_type (
				TREE_TYPE (TREE_OPERAND (rhs, 0)));
      srfield *new_field = type->find_field (
				int_byte_position (TREE_OPERAND (rhs, 1)));
      tree pointer_base = get_true_pointer_base (
				gsi, TREE_OPERAND (newrhs, 0), type);
      newrhs = rewrite_address (pointer_base, new_field, type, gsi);
    }
  return false;
}

bool
ipa_struct_reorg::rewrite_assign (gassign *stmt, gimple_stmt_iterator *gsi)
{
  bool remove = false;

  if (current_layout_opt_level & DEAD_FIELD_ELIMINATION
      && remove_dead_field_stmt (gimple_assign_lhs (stmt)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n rewriting statement (remove): \n");
	  print_gimple_stmt (dump_file, stmt, 0);
	}
      /* Replace the dead field in stmt by creating a dummy ssa.  */
      tree dummy_ssa = make_ssa_name (TREE_TYPE (gimple_assign_lhs (stmt)));
      gimple_assign_set_lhs (stmt, dummy_ssa);
      update_stmt (stmt);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "To: \n");
	  print_gimple_stmt (dump_file, stmt, 0);
	}
    }

  if (gimple_clobber_p (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree newlhs[max_split];
      if (!rewrite_expr (lhs, newlhs))
	return false;
      for (unsigned i = 0; i < max_split && newlhs[i]; i++)
	{
	  tree clobber = build_constructor (TREE_TYPE (newlhs[i]), NULL);
	  TREE_THIS_VOLATILE (clobber) = true;
	  gimple *newstmt = gimple_build_assign (newlhs[i], clobber);
	  gsi_insert_before (gsi, newstmt, GSI_SAME_STMT);
	  remove = true;
	}
      return remove;
    }

  if ((current_layout_opt_level < STRUCT_REORDER_FIELDS
       && (gimple_assign_rhs_code (stmt) == EQ_EXPR
	   || gimple_assign_rhs_code (stmt) == NE_EXPR))
      || (current_layout_opt_level >= STRUCT_REORDER_FIELDS
	  && (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt))
	      == tcc_comparison)))
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree newrhs1[max_split];
      tree newrhs2[max_split];
      tree_code rhs_code = gimple_assign_rhs_code (stmt);
      tree_code code = rhs_code == EQ_EXPR ? BIT_AND_EXPR : BIT_IOR_EXPR;
      if (current_layout_opt_level >= STRUCT_REORDER_FIELDS
	  && rhs_code != EQ_EXPR && rhs_code != NE_EXPR)
	code = rhs_code;

      if (!rewrite_lhs_rhs (rhs1, rhs2, newrhs1, newrhs2))
	return false;
      tree newexpr = NULL_TREE;
      for (unsigned i = 0; i < max_split && newrhs1[i]; i++)
	{
	  tree expr = gimplify_build2 (gsi, rhs_code, boolean_type_node,
				       newrhs1[i], newrhs2[i]);
	  if (!newexpr)
	    newexpr = expr;
	  else
	    newexpr = gimplify_build2 (gsi, code, boolean_type_node,
				       newexpr, expr);
	}

      if (newexpr)
	{
	  newexpr = fold_convert (TREE_TYPE (gimple_assign_lhs (stmt)),
				  newexpr);
	  gimple_assign_set_rhs_from_tree (gsi, newexpr);
	  update_stmt (stmt);
	}
      return false;
    }

  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree newlhs[max_split];
      tree newrhs[max_split];

      if (!rewrite_lhs_rhs (lhs, rhs1, newlhs, newrhs))
	return false;
      tree struct_type = TREE_TYPE (TREE_TYPE (lhs));
      tree size = TYPE_SIZE_UNIT (struct_type);
      tree num = NULL_TREE;
      /* Check if rhs2 is a multiplication of the size of the type.  */
      if ((current_fc_level != fc_level::DYNAMIC
	   || !POINTER_TYPE_P (struct_type))
	  && !is_result_of_mult (rhs2, &num, size)
	  && !(current_layout_opt_level & SEMI_RELAYOUT))
	internal_error (
	  "The rhs of pointer is not a multiplicate and it slips through");

      /* Add the judgment of num, support for POINTER_DIFF_EXPR.
	 _6 = _4 + _5;
	 _5 = (long unsigned int) _3;
	 _3 = _1 - old_2.  */
      if (current_layout_opt_level < STRUCT_REORDER_FIELDS
	  || (current_layout_opt_level >= STRUCT_REORDER_FIELDS
	      && (num != NULL)))
	num = gimplify_build1 (gsi, NOP_EXPR, sizetype, num);
      for (unsigned i = 0; i < max_split && newlhs[i]; i++)
	{
	  gimple *new_stmt;

	  if (num != NULL)
	    {
	      tree newsize = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (newlhs[i])));
	      newsize = gimplify_build2 (gsi, MULT_EXPR, sizetype, num,
					 newsize);
	      if (current_layout_opt_level >= SEMI_RELAYOUT)
		{
		  if (is_semi_relayout_candidate (lhs))
		    {
		      srtype *type = get_semi_relayout_candidate_type (lhs);
		      newrhs[i] = rewrite_pointer_plus_integer (stmt, gsi,
								newrhs[i],
								num, type);
		      newsize = build_int_cst (long_unsigned_type_node, 0);
		    }
		}
	      new_stmt = gimple_build_assign (newlhs[i], POINTER_PLUS_EXPR,
					      newrhs[i], newsize);
	    }
	  else
	    {
	      /* rhs2 is not a const integer  */
	      if (current_layout_opt_level >= SEMI_RELAYOUT)
		{
		  if (is_semi_relayout_candidate (lhs))
		    {
		      num = build_div_expr (gsi, rhs2,
					    build_int_cst (
					    long_unsigned_type_node, 1));
		      srtype *type = get_semi_relayout_candidate_type (lhs);
		      newrhs[i] = rewrite_pointer_plus_integer (stmt,
								gsi, newrhs[i], num, type);
		      rhs2 = build_int_cst (long_unsigned_type_node, 0);
		    }
		}
	      new_stmt = gimple_build_assign (newlhs[i], POINTER_PLUS_EXPR,
					      newrhs[i], rhs2);
	    }
	  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
	  remove = true;
	}
      return remove;
    }

  /* Support POINTER_DIFF_EXPR rewriting.  */
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS
      && gimple_assign_rhs_code (stmt) == POINTER_DIFF_EXPR)
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree newrhs1[max_split];
      tree newrhs2[max_split];

      bool r1 = rewrite_expr (rhs1, newrhs1);
      bool r2 = rewrite_expr (rhs2, newrhs2);

      if (r1 != r2)
	{
	  /* Handle NULL pointer specially.  */
	  if (r1 && !r2 && integer_zerop (rhs2))
	    {
	      r2 = true;
	      for (unsigned i = 0; i < max_split && newrhs1[i]; i++)
		newrhs2[i] = fold_convert (TREE_TYPE (newrhs1[i]), rhs2);
	    }
	  else if (r2 && !r1 && integer_zerop (rhs1))
	    {
	      r1 = true;
	      for (unsigned i = 0; i < max_split && newrhs2[i]; i++)
		newrhs1[i] = fold_convert (TREE_TYPE (newrhs2[i]), rhs1);
	    }
	  else
	    return false;
	}
      else if (!r1 && !r2)
	return false;

      /* The two operands always have pointer/reference type.  */
      if (current_layout_opt_level >= SEMI_RELAYOUT
	  && (is_semi_relayout_candidate (rhs1) || is_semi_relayout_candidate (rhs2)))
	{
	  for (unsigned i = 0; i < max_split && newrhs1[i] &&newrhs2[i]; i++)
	    {
	      srtype *type = get_semi_relayout_candidate_type (rhs1);
	      if (!type)
		{
		  type = get_semi_relayout_candidate_type (rhs2);
		}
	      gcc_assert (type != NULL);
	      tree res = rewrite_pointer_diff (gsi, newrhs1[i],
					       newrhs2[i], type);
	      gimple *g = gimple_build_assign (gimple_assign_lhs (stmt),
					       res);
	      gsi_insert_before (gsi, g, GSI_SAME_STMT);
	    }
	  remove = true;
	}
      else
	{
	  for (unsigned i = 0; i < max_split && newrhs1[i] && newrhs2[i]; i++)
	    {
	      gimple_assign_set_rhs1 (stmt, newrhs1[i]);
	      gimple_assign_set_rhs2 (stmt, newrhs2[i]);
	      update_stmt (stmt);
	    }
	}
      return remove;
    }

  if (gimple_assign_rhs_class (stmt) == GIMPLE_SINGLE_RHS)
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs = gimple_assign_rhs1 (stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nrewriting statement:\n");
	  print_gimple_stmt (dump_file, stmt, 0);
	}
      tree newlhs[max_split];
      tree newrhs[max_split];
      cur_srfd = NULL;
      if (!rewrite_lhs_rhs (lhs, rhs, newlhs, newrhs))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nDid nothing to statement.\n");
	  return false;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\nreplaced with:\n");
      for (unsigned i = 0; i < max_split && (newlhs[i] || newrhs[i]); i++)
	{
	  bool fields_copied = false;
	  if (current_layout_opt_level & SEMI_RELAYOUT)
	    fields_copied = do_semi_relayout (gsi, stmt, newlhs[i], newrhs[i]);
	  if (current_layout_opt_level >= POINTER_COMPRESSION_SAFE)
	    try_rewrite_with_pointer_compression (stmt, gsi, lhs, rhs,
						  newlhs[i], newrhs[i]);
	  if (current_fc_level == fc_level::DYNAMIC
	      && cur_srfd && cur_srfd->dfc_type_change_p ())
	    dynamic_fc_rewrite_assign (stmt, rhs, newlhs[i], newrhs[i]);
	  remove = true;
	  if (fields_copied)
	    continue;
	  tree lhs_expr = newlhs[i] ? newlhs[i] : lhs;
	  tree rhs_expr = newrhs[i] ? newrhs[i] : rhs;

	  tree conv_rhs = build_convert_gimple (lhs_expr, rhs_expr, gsi);
	  if (conv_rhs)
	    rhs_expr = conv_rhs;
	  if (rhs_expr && get_gimple_rhs_class (TREE_CODE (rhs_expr))
			  == GIMPLE_INVALID_RHS)
	    rhs_expr = gimplify_build1 (gsi, NOP_EXPR, TREE_TYPE (rhs_expr),
					rhs_expr);
	  gimple *newstmt = gimple_build_assign (lhs_expr, rhs_expr);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      print_gimple_stmt (dump_file, newstmt, 0);
	      fprintf (dump_file, "\n");
	    }
	  gsi_insert_before (gsi, newstmt, GSI_SAME_STMT);
	}
      return remove;
    }

  if (gimple_assign_cast_p (stmt))
    {
      tree rhs = gimple_assign_rhs1 (stmt);
      tree newrhs[max_split];
      if (!rewrite_expr (rhs, newrhs))
	return false;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nrewriting cast statement:\n");
	  print_gimple_stmt (dump_file, stmt, 0);
	}

      tree lhs = gimple_assign_lhs (stmt);
      tree conv_rhs = fold_convert (TREE_TYPE (lhs), newrhs[0]);
      gimple *newstmt = gimple_build_assign (lhs, conv_rhs);
      gsi_insert_before (gsi, newstmt, GSI_SAME_STMT);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "replaced with:\n");
	  print_gimple_stmt (dump_file, newstmt, 0);
	  fprintf (dump_file, "\n");
	}
      return true;
    }

  return remove;
}

tree
ipa_struct_reorg::get_real_allocated_ptr (tree ptr, gimple_stmt_iterator *gsi)
{
  tree ptr_to_int = fold_convert (long_unsigned_type_node, ptr);
  tree align = build_int_cst (long_unsigned_type_node, relayout_part_size);
  tree real_addr = gimplify_build2 (gsi, MINUS_EXPR, long_unsigned_type_node,
				    ptr_to_int, align);
  tree res = gimplify_build1 (gsi, NOP_EXPR,
			      build_pointer_type (long_unsigned_type_node),
			      real_addr);
  return res;
}

tree
ipa_struct_reorg::set_ptr_for_use (tree ptr, gimple_stmt_iterator *gsi)
{
  tree ptr_to_int = fold_convert (long_unsigned_type_node, ptr);
  tree align = build_int_cst (long_unsigned_type_node, relayout_part_size);
  tree ptr_int = gimplify_build2 (gsi, PLUS_EXPR, long_unsigned_type_node,
				  ptr_to_int, align);
  tree res = gimplify_build1 (gsi, NOP_EXPR,
			      build_pointer_type (long_unsigned_type_node),
			      ptr_int);
  return res;
}

void
ipa_struct_reorg::record_allocated_size (tree ptr, gimple_stmt_iterator *gsi,
					 tree size)
{
  tree lhs = fold_build2 (MEM_REF, long_unsigned_type_node, ptr,
			  build_int_cst (build_pointer_type (
			  long_unsigned_type_node), 0));
  gimple *stmt = gimple_build_assign (lhs, size);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
}

tree
ipa_struct_reorg::read_allocated_size (tree ptr, gimple_stmt_iterator *gsi)
{
  tree to_type = build_pointer_type (long_unsigned_type_node);
  tree off = build_int_cst (to_type, 0);
  tree size = gimplify_build2 (gsi, MEM_REF, long_unsigned_type_node,
			       ptr, off);
  return size;
}

gimple *
ipa_struct_reorg::create_aligned_alloc (gimple_stmt_iterator *gsi,
					srtype *type, tree num, tree &size)
{
  tree fn = builtin_decl_implicit (BUILT_IN_ALIGNED_ALLOC);

  tree align = build_int_cst (long_unsigned_type_node, relayout_part_size);
  unsigned bucket_size = type->bucket_size;

  tree nbuckets = gimplify_build2 (gsi, CEIL_DIV_EXPR, long_unsigned_type_node,
				   num, build_int_cst (long_unsigned_type_node,
				   relayout_part_size / 8));
  tree use_size = gimplify_build2 (gsi, MULT_EXPR, long_unsigned_type_node,
				   nbuckets, build_int_cst (
				   long_unsigned_type_node, bucket_size));
  size = gimplify_build2 (gsi, PLUS_EXPR, long_unsigned_type_node,
			  use_size, align);
  gimple *g = gimple_build_call (fn, 2, align, size);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  return g;
}

void
ipa_struct_reorg::create_memset_zero (tree ptr, gimple_stmt_iterator *gsi,
				      tree size)
{
  tree fn = builtin_decl_implicit (BUILT_IN_MEMSET);
  tree val = build_int_cst (long_unsigned_type_node, 0);
  gimple *g = gimple_build_call (fn, 3, ptr, val, size);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
}

void
ipa_struct_reorg::create_memcpy (tree src, tree dst, tree size,
				 gimple_stmt_iterator *gsi)
{
  tree fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
  gimple *g = gimple_build_call (fn, 3, dst, src, size);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
}

void
ipa_struct_reorg::create_free (tree ptr, gimple_stmt_iterator *gsi)
{
  tree fn = builtin_decl_implicit (BUILT_IN_FREE);
  gimple *g = gimple_build_call (fn, 1, ptr);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
}

void
ipa_struct_reorg::copy_to_lhs (tree lhs, tree new_lhs,
			       gimple_stmt_iterator *gsi)
{
  gimple *g = gimple_build_assign (lhs, new_lhs);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
}

/* Rewrite function call statement STMT.  Return TRUE if the statement
   is to be removed.  */

bool
ipa_struct_reorg::rewrite_call (gcall *stmt, gimple_stmt_iterator *gsi)
{
  /* Handled allocation calls are handled seperately from normal
     function calls.  */
  if (handled_allocation_stmt (stmt))
    {
      tree lhs = gimple_call_lhs (stmt);
      tree newrhs1[max_split];
      srdecl *decl = find_decl (lhs);
      if (!decl || !decl->type)
	return false;
      srtype *type = decl->type;
      if (type->has_escaped () || !type->has_new_type ())
	return false;

      tree num = allocate_size (type, decl, stmt);
      gcc_assert (num);
      memset (newrhs1, 0, sizeof (newrhs1));

      /* The realloc call needs to have its first argument rewritten.  */
      if (gimple_call_builtin_p (stmt, BUILT_IN_REALLOC))
	{
	  tree rhs1 = gimple_call_arg (stmt, 0);
	  if (integer_zerop (rhs1))
	    {
	      for (unsigned i = 0; i < max_split; i++)
		newrhs1[i] = rhs1;
	    }
	  else if (!rewrite_expr (rhs1, newrhs1))
	    internal_error ("Rewrite failed for realloc");
	}

      /* Go through each new lhs.  */
      for (unsigned i = 0; i < max_split && decl->newdecl[i]; i++)
	{
	  /* Specify the correct size for the multi-layer pointer.  */
	  tree newsize = isptrptr (decl->orig_type)
			 ? TYPE_SIZE_UNIT (decl->orig_type)
			 : TYPE_SIZE_UNIT (type->newtype[i]);
	  gimple *g;
	  bool rewrite = false;
	  if (current_layout_opt_level >= SEMI_RELAYOUT
	      && type->semi_relayout)
	    {
	      if (gimple_call_builtin_p (stmt, BUILT_IN_MALLOC))
		;
	      else if (gimple_call_builtin_p (stmt, BUILT_IN_CALLOC))
		{
		  tree rhs2 = gimple_call_arg (stmt, 1);
		  if (tree_to_uhwi (rhs2) == tree_to_uhwi (TYPE_SIZE_UNIT (type->type)))
		    {
		      rewrite = true;
		      tree size = NULL_TREE;
		      g = create_aligned_alloc (gsi, type, num, size);
		      tree real_ptr = make_ssa_name (build_pointer_type (unsigned_char_type_node));
		      gimple_set_lhs (g, real_ptr);
		      create_memset_zero (real_ptr, gsi, size);
		      record_allocated_size (real_ptr, gsi, size);
		      tree lhs_use = set_ptr_for_use (real_ptr, gsi);
		      copy_to_lhs (decl->newdecl[i], lhs_use, gsi);
		    }
		}
	      else if (gimple_call_builtin_p (stmt, BUILT_IN_REALLOC))
		{
		  rewrite = true;
		  tree size = NULL_TREE;
		  g = create_aligned_alloc (gsi, type, num, size);
		  tree real_ptr = make_ssa_name (build_pointer_type (unsigned_char_type_node));
		  gimple_set_lhs (g, real_ptr);
		  create_memset_zero (real_ptr, gsi, size);
		  tree src = get_real_allocated_ptr (newrhs1[i], gsi);
		  tree old_size = read_allocated_size (src, gsi);
		  create_memcpy (src, real_ptr, old_size, gsi);
		  record_allocated_size (real_ptr, gsi, size);
		  tree lhs_use = set_ptr_for_use (real_ptr, gsi);
		  create_free (src, gsi);
		  copy_to_lhs (decl->newdecl[i], lhs_use, gsi);
		}
	      else
		{
		  gcc_assert (false);
		  internal_error ("supported type for semi-relayout.");
		}
	    }
	  if (!rewrite
	      && (current_layout_opt_level >= STRUCT_REORDER_FIELDS
		  || current_layout_opt_level == STRUCT_SPLIT))
	    {
	      /* Every allocation except for calloc needs the size multiplied out.  */
	      if (!gimple_call_builtin_p (stmt, BUILT_IN_CALLOC))
		newsize = gimplify_build2 (gsi, MULT_EXPR, sizetype,
					   num, newsize);
	      if (gimple_call_builtin_p (stmt, BUILT_IN_MALLOC)
		  || gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA))
		g = gimple_build_call (gimple_call_fndecl (stmt), 1, newsize);
	      else if (gimple_call_builtin_p (stmt, BUILT_IN_CALLOC))
		g = gimple_build_call (gimple_call_fndecl (stmt), 2,
				       num, newsize);
	      else if (gimple_call_builtin_p (stmt, BUILT_IN_REALLOC))
		g = gimple_build_call (gimple_call_fndecl (stmt), 2,
				       newrhs1[i], newsize);
	      else
		gcc_assert (false);
	      gimple_call_set_lhs (g, decl->newdecl[i]);
	      gsi_insert_before (gsi, g, GSI_SAME_STMT);
	    }


	  if (type->pc_candidate)
	    {
	      /* Init global header for pointer compression.  */
	      gassign *gptr
		= gimple_build_assign (type->pc_gptr, decl->newdecl[i]);
	      gsi_insert_before (gsi, gptr, GSI_SAME_STMT);
	    }
	}
      return true;
    }

  /* The function call free needs to be handled special.  */
  if (gimple_call_builtin_p (stmt, BUILT_IN_FREE))
    {
      tree expr = gimple_call_arg (stmt, 0);
      tree newexpr[max_split];
      if (!rewrite_expr (expr, newexpr))
	return false;

      srtype *t = find_type (TREE_TYPE (TREE_TYPE (expr)));
      if (newexpr[1] == NULL)
	{
	  if (t && t->semi_relayout)
	    newexpr[0] = get_real_allocated_ptr (newexpr[0], gsi);
	  gimple_call_set_arg (stmt, 0, newexpr[0]);
	  update_stmt (stmt);
	  return false;
	}

      for (unsigned i = 0; i < max_split && newexpr[i]; i++)
	{
	  gimple *g = gimple_build_call (gimple_call_fndecl (stmt),
					 1, newexpr[i]);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	}
      return true;
    }

  /* Otherwise, look up the function to see if we have cloned it
     and rewrite the arguments.  */
  tree fndecl = gimple_call_fndecl (stmt);

  /* Indirect calls are already marked as escaping so ignore.  */
  if (!fndecl)
    return false;

  cgraph_node *node = cgraph_node::get (fndecl);
  gcc_assert (node);
  srfunction *f = find_function (node);

  /* Add a safe func mechanism.  */
  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS
      && current_fc_level != fc_level::DYNAMIC
      && f && f->is_safe_func)
    {
      tree expr = gimple_call_arg (stmt, 0);
      tree newexpr[max_split];
      if (!rewrite_expr (expr, newexpr))
	return false;

      if (newexpr[1] == NULL)
	{
	  gimple_call_set_arg (stmt, 0, newexpr[0]);
	  update_stmt (stmt);
	  return false;
	}
      return false;
    }

  /* Did not find the function or had not cloned it return saying don't
     change the function call.  */
  if (!f)
    return false;

  if (current_fc_level == fc_level::DYNAMIC)
    {
      if (f->partial_clone_p ())
	return false;
      f = f->fc_path.cloned_func;
    }
  else
    {
      if (!f->newf)
	return false;
      /* Move over to the new function.  */
      f = f->newf;
    }

  if (current_fc_level == fc_level::DYNAMIC && f->is_safe_func)
    {
      tree expr = gimple_call_arg (stmt, 0);
      tree newexpr[max_split] = {NULL_TREE};
      if (rewrite_expr (expr, newexpr) && newexpr[1] == NULL_TREE)
	gimple_call_set_arg (stmt, 0, newexpr[0]);

      gimple_call_set_fndecl (stmt, f->node->decl);
      update_stmt (stmt);
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Changing arguments for function call :\n");
      print_gimple_expr (dump_file, stmt, 0);
      fprintf (dump_file, "\n");
    }

  tree chain = gimple_call_chain (stmt);
  unsigned nargs = gimple_call_num_args (stmt);
  auto_vec<tree> vargs (nargs);

  if (chain)
    {
      tree newchains[max_split];
      if (rewrite_expr (chain, newchains))
	{
	  /* Chain decl's type cannot be split and but it can change.  */
	  gcc_assert (newchains[1] == NULL);
	  chain = newchains[0];
	}
    }

  for (unsigned i = 0; i < nargs; i++)
    vargs.quick_push (gimple_call_arg (stmt, i));

  int extraargs = 0;

  for (unsigned i = 0; i < f->args.length (); i++)
    {
      srdecl *d = f->args[i];
      if (d->argumentnum == -2)
	continue;
      gcc_assert (d->argumentnum != -1);
      tree arg = vargs[d->argumentnum + extraargs];
      tree newargs[max_split];
      if (!rewrite_expr (arg, newargs))
	continue;

      /* If this ARG has a replacement handle the replacement.  */
      for (unsigned j = 0; j < max_split && d->newdecl[j]; j++)
	{
	  gcc_assert (newargs[j]);
	  /* If this is the first replacement of the arugment,
	     then just replace it.  */
	  if (j == 0)
	    vargs[d->argumentnum + extraargs] = newargs[j];
	  else
	    {
	      /* More than one replacement,
		 we need to insert into the array.  */
	      extraargs++;
	      vargs.safe_insert (d->argumentnum + extraargs, newargs[j]);
	    }
	}
    }

  gcall *new_stmt;

  new_stmt = gimple_build_call_vec (f->node->decl, vargs);

  if (gimple_call_lhs (stmt))
    gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));

  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
  gimple_set_vdef (new_stmt, gimple_vdef (stmt));

  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_copy_flags (new_stmt, stmt);
  gimple_call_set_chain (new_stmt, chain);

  gimple_set_modified (new_stmt, true);

  if (gimple_vdef (new_stmt)
      && TREE_CODE (gimple_vdef (new_stmt)) == SSA_NAME)
    SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;

  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);

  /* We need to defer cleaning EH info on the new statement to
     fixup-cfg.  We may not have dominator information at this point
     and thus would end up with unreachable blocks and have no way
     to communicate that we need to run CFG cleanup then.  */
  int lp_nr = lookup_stmt_eh_lp (stmt);
  if (lp_nr != 0)
    {
      remove_stmt_from_eh_lp (stmt);
      add_stmt_to_eh_lp (new_stmt, lp_nr);
    }

  return true;
}

/* Rewrite the conditional statement STMT.  Return TRUE if the
   old statement is to be removed.  */

bool
ipa_struct_reorg::rewrite_cond (gcond *stmt,
				gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED)
{
  tree_code rhs_code = gimple_cond_code (stmt);

  /* Handle only equals or not equals conditionals.  */
  if ((current_layout_opt_level < STRUCT_REORDER_FIELDS
       && (rhs_code != EQ_EXPR && rhs_code != NE_EXPR))
      || (current_layout_opt_level >= STRUCT_REORDER_FIELDS
	  && TREE_CODE_CLASS (rhs_code) != tcc_comparison))
    return false;
  tree lhs = gimple_cond_lhs (stmt);
  tree rhs = gimple_cond_rhs (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nCOND: Rewriting\n");
      print_gimple_stmt (dump_file, stmt, 0);
      print_generic_expr (dump_file, lhs);
      fprintf (dump_file, "\n");
      print_generic_expr (dump_file, rhs);
      fprintf (dump_file, "\n");
    }

  tree newlhs[max_split] = {};
  tree newrhs[max_split] = {};
  if (!rewrite_lhs_rhs (lhs, rhs, newlhs, newrhs))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Did nothing to statement.\n");
      return false;
    }

  /*  Old rewrite: if (x_1 != 0B)
		-> _1 = x.reorder.0_1 != 0B; if (_1 != 1)
		   The logic is incorrect.
      New rewrite: if (x_1 != 0B)
		-> if (x.reorder.0_1 != 0B); */
  for (unsigned i = 0; i < max_split && (newlhs[i] || newrhs[i]); i++)
    {
      if (newlhs[i])
	gimple_cond_set_lhs (stmt, newlhs[i]);
      if (newrhs[i])
	gimple_cond_set_rhs (stmt, newrhs[i]);
      update_stmt (stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
      {
	fprintf (dump_file, "replaced with:\n");
	print_gimple_stmt (dump_file, stmt, 0);
	fprintf (dump_file, "\n");
      }
    }
  return false;
}

/* Rewrite debug statments if possible.  Return TRUE if the statement
   should be removed.  */

bool
ipa_struct_reorg::rewrite_debug (gimple *, gimple_stmt_iterator *)
{
  /* In debug statements, there might be some statements that have
     been optimized out in gimple but left in debug gimple.  Sometimes
     these statements need to be analyzed to escape, but in rewrite
     stage it shouldn't happen.  It needs to care a lot to handle these
     cases but seems useless.  So now we just delete debug gimple.  */
  return true;
}

/* Rewrite PHI nodes, return true if the PHI was replaced.  */

bool
ipa_struct_reorg::rewrite_phi (gphi *phi)
{
  tree newlhs[max_split];
  gphi *newphi[max_split];
  tree result = gimple_phi_result (phi);
  gphi_iterator gsi;

  memset (newphi, 0, sizeof (newphi));

  if (!rewrite_expr (result, newlhs))
    return false;

  if (newlhs[0] == NULL)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nrewriting PHI:\n");
      print_gimple_stmt (dump_file, phi, 0);
    }

  for (unsigned i = 0; i < max_split && newlhs[i]; i++)
    newphi[i] = create_phi_node (newlhs[i], gimple_bb (phi));

  for (unsigned i = 0; i  < gimple_phi_num_args (phi); i++)
    {
      tree newrhs[max_split];
      phi_arg_d rhs = *gimple_phi_arg (phi, i);

      /* Handling the NULL phi Node.  */
      bool r = rewrite_expr (rhs.def, newrhs);
      if (!r && integer_zerop (rhs.def))
	{
	  for (unsigned i = 0; i < max_split && newlhs[i]; i++)
	    newrhs[i] = fold_convert (TREE_TYPE (newlhs[i]), rhs.def);
	}

      for (unsigned j = 0; j < max_split && newlhs[j]; j++)
	{
	  SET_PHI_ARG_DEF (newphi[j], i, newrhs[j]);
	  gimple_phi_arg_set_location (newphi[j], i, rhs.locus);
	  update_stmt (newphi[j]);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "into:\n");
      for (unsigned i = 0; i < max_split && newlhs[i]; i++)
	{
	  print_gimple_stmt (dump_file, newphi[i], 0);
	  fprintf (dump_file, "\n");
	}
    }

  gsi = gsi_for_phi (phi);
  remove_phi_node (&gsi, false);
  release_ssa_names.safe_push (gimple_phi_result (phi));

  return true;
}

/* Rewrite gimple statement STMT, return true if the STATEMENT
   is to be removed.  */

bool
ipa_struct_reorg::rewrite_stmt (gimple *stmt, gimple_stmt_iterator *gsi)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      return rewrite_assign (as_a <gassign *> (stmt), gsi);
    case GIMPLE_CALL:
      return rewrite_call (as_a <gcall *> (stmt), gsi);
    case GIMPLE_COND:
      return rewrite_cond (as_a <gcond *> (stmt), gsi);
      break;
    case GIMPLE_GOTO:
    case GIMPLE_SWITCH:
      break;
    case GIMPLE_DEBUG:
    case GIMPLE_ASM:
      break;
    default:
      break;
    }
  return false;
}

/* Does the function F uses any decl which has changed.  */

bool
ipa_struct_reorg::has_rewritten_type (srfunction *f)
{
  for (unsigned i = 0; i < f->decls.length (); i++)
    {
      srdecl *d = f->decls[i];
      if (d->newdecl[0] != d->decl)
	return true;
    }

  for (unsigned i = 0; i < f->globals.length (); i++)
    {
      srdecl *d = f->globals[i];
      if (d->newdecl[0] != d->decl)
	return true;
    }
  return false;
}

/* Rewrite the functions if needed, return
   the TODOs requested.  */

unsigned
ipa_struct_reorg::rewrite_functions (void)
{
  unsigned retval = 0;

  if (flag_ipa_struct_sfc_shadow)
    {
      for (auto *info : fc_infos)
	{
	  if (!info || !info->static_fc_p)
	    continue;
	  cleanup_shadow_write (info);
	  rewrite_shadow_read (info);
	}
    }

  /* Create new types, if we did not create any new types,
     then don't rewrite any accesses.  */
  if (!create_new_types ())
    {
      if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	{
	  for (unsigned i = 0; i < functions.length (); i++)
	    {
	      srfunction *f = functions[i];
	      cgraph_node *node = f->node;
	      push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "\nNo rewrite:\n");
		  if (current_function_decl == NULL)
		    {
		      fprintf (dump_file, "\ncurrent_function_decl == NULL\n");
		      continue;
		    }
		  if (current_function_decl)
		    dump_function_to_file (current_function_decl, dump_file,
					   dump_flags | TDF_VOPS);
		  else
		    fprintf (dump_file, " no declaration\n");
		}
	      pop_cfun ();
	    }
	}
      return 0;
    }

  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS && dump_file)
    {
      fprintf (dump_file, "=========== all created newtypes: ===========\n\n");
      dump_newtypes (dump_file);
    }

  if (functions.length ())
    {
      retval = TODO_remove_functions;
      create_new_functions ();
      if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
	{
	  prune_escaped_types ();
	}
    }

  if (current_layout_opt_level >= STRUCT_REORDER_FIELDS)
    {
      for (unsigned i = 0; i < functions.length (); i++)
	{
	  srfunction *f = functions[i];
	  cgraph_node *node = f->node;
	  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "==== Before create decls: %dth %s ====\n\n",
		       i, f->node->name ());
	      if (current_function_decl)
		dump_function_to_file (current_function_decl, dump_file,
				       dump_flags | TDF_VOPS);
	      else
		fprintf (dump_file, " no declaration\n");
	    }
	  pop_cfun ();
	}
    }

  create_new_decls ();

  for (unsigned i = 0; i < functions.length (); i++)
    {
      srfunction *f = functions[i];
      if (f->newnode)
	continue;

      /* Function uses no rewriten types so don't cause a rewrite.  */
      if (!has_rewritten_type (f))
	continue;

      cgraph_node *node = f->node;
      basic_block bb;

      push_cfun (DECL_STRUCT_FUNCTION (node->decl));
      current_function = f;

      if (current_layout_opt_level >= POINTER_COMPRESSION_SAFE)
	{
	  calculate_dominance_info (CDI_DOMINATORS);
	  loop_optimizer_init (0);
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nBefore rewrite: %dth %s\n",
		   i, f->node->name ());
	  if (current_function_decl)
	    dump_function_to_file (current_function_decl, dump_file,
				   dump_flags | TDF_VOPS);
	  else
	    fprintf (dump_file, " no declaration\n");
	  fprintf (dump_file, "\n======== Start to rewrite: %dth_%s ========\n",
		   i, f->node->name ());
	}
      FOR_EACH_BB_FN (bb, cfun)
	rewrite_block (bb);

      /* Release the old SSA_NAMES for old arguments.  */
      if (f->old)
	{
	  for (unsigned i = 0; i < f->args.length (); i++)
	    {
	      srdecl *d = f->args[i];
	      if (d->newdecl[0] != d->decl)
		{
		  tree ssa_name = ssa_default_def (cfun, d->decl);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Found ");
		      print_generic_expr (dump_file, ssa_name);
		      fprintf (dump_file, " to be released.\n");
		    }
		  release_ssa_name (ssa_name);
		}
	    }
	}
      for (unsigned i = 0; i < release_ssa_names.length (); i++)
	release_ssa_name (release_ssa_names[i]);

      update_ssa (TODO_update_ssa_only_virtuals);

      if (flag_tree_pta)
	compute_may_aliases ();

      remove_unused_locals ();

      cgraph_edge::rebuild_edges ();

      free_dominance_info (CDI_DOMINATORS);

      if (current_layout_opt_level >= POINTER_COMPRESSION_SAFE)
	loop_optimizer_finalize ();

      if (dump_file)
	{
	  fprintf (dump_file, "\nAfter rewrite: %dth %s\n",
		   i, f->node->name ());
	  if (current_function_decl)
	    dump_function_to_file (current_function_decl, dump_file,
				   dump_flags | TDF_VOPS);
	  else
	    fprintf (dump_file, " no declaration\n");
	}

      pop_cfun ();
      current_function = NULL;
    }

  return retval | TODO_verify_all;
}

void
ipa_struct_reorg::rewrite_block (basic_block bb)
{
  for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);)
    {
      if (rewrite_phi (si.phi ()))
	si = gsi_start_phis (bb);
      else
	gsi_next (&si);
    }

  for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);)
    {
      gimple *stmt = gsi_stmt (si);
      if (rewrite_stmt (stmt, &si))
	gsi_remove (&si, true);
      else
	gsi_next (&si);
    }

  /* Debug statements need to happen after all other statements
     have changed.  */
  for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);)
    {
      gimple *stmt = gsi_stmt (si);
      if (gimple_code (stmt) == GIMPLE_DEBUG
	  && rewrite_debug (stmt, &si))
	gsi_remove (&si, true);
      else
	gsi_next (&si);
    }
}

unsigned int
ipa_struct_reorg::execute_struct_relayout (void)
{
  unsigned retval = 0;
  for (unsigned i = 0; i < types.length (); i++)
    {
      tree type = types[i]->type;
      if (TYPE_FIELDS (type) == NULL)
	continue;
      if (types[i]->has_alloc_array != 1)
	continue;
      if (types[i]->chain_type)
	continue;
      if (get_type_name (types[i]->type) == NULL)
	continue;
      retval |= ipa_struct_relayout (type, this).execute ();
    }

  if (dump_file)
    {
      if (transformed)
	fprintf (dump_file, "\nNumber of structures to transform in "
		 "Complete Structure Relayout is %d\n", transformed);
      else
	fprintf (dump_file, "\nNo structures to transform in "
		 "Complete Structure Relayout.\n");
    }

  return retval;
}

/* True if the var with void type is only used to compare with the same
   target type.  */

bool
ipa_struct_reorg::safe_void_cmp_p (tree var, srtype *type)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, var)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;

      if (gimple_code (use_stmt) == GIMPLE_COND)
	{
	  tree lhs = gimple_cond_lhs (use_stmt);
	  tree rhs = gimple_cond_rhs (use_stmt);
	  tree xhs = lhs == var ? rhs : lhs;
	  if (types_compatible_p (inner_type (TREE_TYPE (xhs)), type->type))
	    continue;

	}
      return false;
    }
  return true;
}

/* Mark the structure that should perform pointer compression.  */

void
ipa_struct_reorg::check_and_prune_struct_for_pointer_compression (void)
{
  unsigned pc_transform_num = 0;

  if (dump_file)
    fprintf (dump_file, "\nMark the structure that should perform pointer"
			" compression:\n");

  for (unsigned i = 0; i < types.length (); i++)
    {
      srtype *type = types[i];
      if (dump_file)
	print_generic_expr (dump_file, type->type);

      if (type->has_escaped ())
	{
	  if (dump_file)
	    fprintf (dump_file, " has escaped by %s, skip compression.\n",
		     type->escape_reason ());
	  continue;
	}
      if (TYPE_FIELDS (type->type) == NULL)
	{
	  if (dump_file)
	    fprintf (dump_file, " has zero field, skip compression.\n");
	  continue;
	}
      if (type->chain_type)
	{
	  if (dump_file)
	      fprintf (dump_file, " is chain_type, skip compression.\n");
	  continue;
	}
      if (type->has_alloc_array != 1)
	{
	  if (dump_file)
	    fprintf (dump_file, " has alloc number: %d, skip compression.\n",
		     type->has_alloc_array);
	  continue;
	}
      if (get_type_name (type->type) == NULL)
	{
	  if (dump_file)
	    fprintf (dump_file, " has empty struct name,"
				" skip compression.\n");
	  continue;
	}
      if (!type->has_legal_alloc_num)
	{
	  if (current_layout_opt_level & POINTER_COMPRESSION_UNSAFE)
	    {
	    if (dump_file)
	      fprintf (dump_file, " has unknown alloc size, but"
				  " in unsafe mode, so");
	    }
	  else
	    {
	      if (dump_file)
		fprintf (dump_file, " has illegal struct array size,"
				    " skip compression.\n");
	      continue;
	    }
	}
      pc_transform_num++;
      type->pc_candidate = true;
      if (dump_file)
	fprintf (dump_file, " attempts to do pointer compression.\n");
    }

  if (dump_file)
    {
      if (pc_transform_num)
	fprintf (dump_file, "\nNumber of structures to transform in "
			    "pointer compression is %d\n", pc_transform_num);
      else
	fprintf (dump_file, "\nNo structures to transform in "
			    "pointer compression.\n");
    }
}

void
ipa_struct_reorg::check_and_prune_struct_for_semi_relayout (void)
{
  unsigned relayout_transform = 0;
  for (unsigned i = 0; i < types.length (); i++)
    {
      srtype *type = types[i];
      if (dump_file)
	{
	  print_generic_expr (dump_file, type->type);
	}
      if (type->has_escaped ())
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, " has escaped by %s, "
				  "skip relayout.\n", type->escape_reason());
	    }
	  continue;
	}
      if (TYPE_FIELDS (type->type) == NULL)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, " has zero field, skip relayout.\n");
	    }
	  continue;
	}
      if (type->chain_type)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, " is chain_type, skip relayout.\n");
	    }
	    continue;
	}
      if (type->has_alloc_array == 0 || type->has_alloc_array == 1
	  || type->has_alloc_array == -1 || type->has_alloc_array == -3
	  || type->has_alloc_array == -4)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, " has alloc number: %d,"
				  " skip relayout.\n", type->has_alloc_array);
	    }
	  continue;
	}
      if (get_type_name (type->type) == NULL)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, " has empty struct name,"
				  " skip relayout.\n");
	    }
	  continue;
	}
      relayout_transform++;
      type->semi_relayout = true;
      if (dump_file)
	{
	  fprintf (dump_file, " attempts to do semi-relayout.\n");
	}
    }

  if (dump_file)
    {
      if (relayout_transform)
	{
	  fprintf (dump_file, "\nNumber of structures to transform in "
			      "semi-relayout is %d\n", relayout_transform);
	}
      else
	{
	  fprintf (dump_file, "\nNo structures to transform in "
			      "semi-relayout.\n");
	}
    }
}

/* Get the BASE and field of the VAR.  */
bool
ipa_struct_reorg::get_base_type (tree var, tree &base,
				 srtype *&type, srfield *&field)
{
  if (!var || TREE_CODE (var) != COMPONENT_REF)
    return false;

  /* Ignore data access that is canonical.  */
  bool realpart, imagpart;
  bool address;
  bool indirect;
  bool escape_from_base;
  return get_type_field (var, base, indirect, type, field,
			 realpart, imagpart, address, escape_from_base);
}

void
ipa_struct_reorg::check_and_prune_struct_for_field_compression (void)
{
  for (auto *type : types)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "[field compress] Analyzing type : ");
	  print_generic_expr (dump_file, type->type);
	  fprintf (dump_file, "\n");
	}

      /* Check if the type is escaped or not.  */
      if (type->has_escaped ())
	continue;

      type->fc_info = new fc_type_info (type);
      fc_infos.safe_push (type->fc_info);

      if (!find_field_compression_candidate (type))
	continue;

      gcc_assert (type->fc_info->static_fc_p ^ type->fc_info->dynamic_fc_p);
      if (dump_file)
	{
	  fprintf (dump_file, "[field compress] Found candidate: ");
	  print_generic_expr (dump_file, type->type);
	  fprintf (dump_file, "\n");
	}

      /* Support only 1 type.  */
      break;
    }
}

/* Find a field compression candidate.  */

bool
ipa_struct_reorg::find_field_compression_candidate (srtype *type)
{
  if (type->has_recursive_field_type ())
    {
      FC_DUMP_MSG ("Recursive field type unsupported\n");
      return false;
    }

  fc_type_info *info = type->fc_info;

  /* Classify fields by field type firstly.  */
  classify_fields (info);

  if (current_fc_level == fc_level::STATIC)
    {
      FC_DUMP_MSG ("Looking for static fc fields\n");
      info->static_fc_p = find_static_fc_fields (info);
    }

  if (current_fc_level == fc_level::DYNAMIC)
    {
      FC_DUMP_MSG ("Looking for dynamic fc fields\n");
      info->dynamic_fc_p = find_dynamic_fc_fields (info);
    }

  if (!info->static_fc_p && !info->dynamic_fc_p)
    {
      FC_DUMP_MSG ("Fail finding field compression candidate\n");
      return false;
    }

  if (!compress_fields (info))
    {
      FC_DUMP_MSG ("Fail compressing fields\n");
      info->static_fc_p = false;
      info->dynamic_fc_p = false;
      return false;
    }

  return true;
}

/* Classify all fields by data type.  */

void
ipa_struct_reorg::classify_fields (fc_type_info *info)
{
  for (auto *srf : info->type->fields)
    if (!srf->dead_field_p ())
      info->record_field_class (srf);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      unsigned i;
      fc_field_class *field_class;
      FOR_EACH_VEC_ELT (info->field_classes, i, field_class)
	{
	  fprintf (dump_file, "[field compress] %dth field class:\n", i);
	  field_class->dump (dump_file);
	}
    }
}

/* Scan all of fields to check whether each can be statically
   compressed or not.  */

bool
ipa_struct_reorg::find_static_fc_fields (fc_type_info *info)
{
  bool found_static_compress = false;

  if (flag_ipa_struct_sfc_shadow)
    found_static_compress |= find_shadow_fields (info);

  for (auto *srf : info->type->fields)
    {
      /* Skip dead field.  */
      if (srf->dead_field_p ())
	continue;

      /* Avoid compressing non-integer type.  */
      if (TREE_CODE (srf->fieldtype) != INTEGER_TYPE)
	continue;

      /* We have marked these fields as shadow, so skip them.  */
      if (find_fc_field (info->static_fc_fields, srf->fielddecl))
	continue;

      found_static_compress |= static_compress_p (info, srf->fielddecl);
    }

  if (!found_static_compress)
    {
      FC_DUMP_MSG ("Fail finding static fc fields\n");
      return false;
    }

  gcc_assert (!info->static_fc_fields.is_empty ());

  /* Avoid compressing fields without hot access.  */
  if (!find_hot_access (info, info->static_fc_fields))
    {
      FC_DUMP_MSG ("Fail finding hot access for static\n");
      return false;
    }

  return true;
}

/* Compress fields and create new field types.  */

bool
ipa_struct_reorg::compress_fields (fc_type_info *info)
{
  gcc_assert (info->static_fc_p ^ info->dynamic_fc_p);

  if (info->static_fc_p)
    {
      return compress_fields_static (info)
	     && compress_to_bitfield_static (info);
    }
  else
    {
      return compress_fields_dynamic (info)
	     && compress_to_bitfield_dynamic (info)
	     && calc_dynamic_boundary (info)
	     && check_closure (info);
    }
}

/* Check if the type has any field that can be shadowed.  */

bool
ipa_struct_reorg::find_shadow_fields (fc_type_info *info)
{
  FC_DUMP_MSG ("Finding shadow fields\n");

  bool found_shadow = false;
  for (auto *field_class : info->field_classes)
    {
      /* Avoid shadowing non-integer type, we can try to do this
	 in the future.  */
      if (TREE_CODE (field_class->fieldtype) != INTEGER_TYPE)
	continue;

      /* Field shadowing requires two or more fields.  */
      if (field_class->size () < 2)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Find shadow field for field class:\n");
	  field_class->dump (dump_file);
	}

      if (find_shadow_fields (info, field_class))
	{
	  found_shadow = true;
	  continue;
	}

      FC_DUMP_MSG ("Fail finding shadow field\n");
    }

  return found_shadow;
}

bool
ipa_struct_reorg::find_shadow_fields (fc_type_info *info,
				      fc_field_class *field_class)
{
  /* Find and record all pair assignments.  */
  fc_shadow_info shadow_info;
  if (!find_pair_stmts (field_class, shadow_info))
    return false;

  /* Unpair assignment checking.  */
  auto &srfields = field_class->srfields;
  unsigned original_index = 0;
  tree init_const = NULL_TREE;
  if (shadow_info.unpair_stmt)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Find unpair assignment:\n");
	  print_gimple_stmt (dump_file, shadow_info.unpair_stmt, 0);
	}

      original_index = shadow_info.unpair_stmt_index;
      if (!check_unpair_stmt (field_class,
			      shadow_info.unpair_stmt,
			      shadow_info.unpair_stmt_func,
			      srfields[original_index]))
	return false;
    }

  if (current_fc_level == fc_level::DYNAMIC)
    {
      if (!shadow_info.unpair_stmt)
	return false;
      /* We have proved that the unpair_stmt is single assign.  */
      init_const = gimple_assign_rhs1 (shadow_info.unpair_stmt);
      if (TREE_CODE (init_const) != INTEGER_CST)
	return false;

      if (!unique_init_const_p (shadow_info))
	return false;
    }

  /* Add a new static fc_field.  */
  srfield *original_srf = srfields[original_index];

  unsigned i;
  srfield *shadow_srf;
  FOR_EACH_VEC_ELT (srfields, i, shadow_srf)
    {
      if (i == original_index)
	continue;

      fc_field *fc_f = new fc_field (shadow_srf->fielddecl, 1, original_srf);
      auto &fc_fields = current_fc_level == fc_level::STATIC
		      ? info->static_fc_fields : info->dynamic_shadow_fields;
      fc_fields.safe_push (fc_f);
      fc_f->init_const = init_const; /* Not NULL only in dynamic.  */

      /* Record all shadow stmts to fc_field.  */
      unsigned j;
      auto_vec<gimple *> *group;
      FOR_EACH_VEC_ELT (shadow_info.pair_stmts_groups, j, group)
	{
	  fc_f->shadow_stmts.safe_push ((*group)[i]);
	  fc_f->shadow_stmts_func.safe_push (shadow_info.pair_stmts_func[j]);
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Found shadow field: ");
	  print_generic_expr (dump_file, shadow_srf->fielddecl);
	  fprintf (dump_file, "\n");
	}
    }

  return true;
}

bool
ipa_struct_reorg::find_pair_stmts (fc_field_class *field_class,
				   fc_shadow_info &info)
{
  for (auto *srfn : functions)
    {
      SET_CFUN (srfn);
      basic_block bb = NULL;
      FOR_EACH_BB_FN (bb, cfun)
	{
	  auto_vec<gimple *> group;
	  auto_vec<unsigned> indexes;
	  auto_bitmap visited_fields;
	  bool read = false;

	  for (auto si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	    {
	      gimple *stmt = gsi_stmt (si);
	      if (!is_gimple_assign (stmt))
		continue;

	      /* Check if any of the fields have been read.
		 We need to make sure there is no stmt
		 reading the fields within a pair stmt group.  */
	      if (read_field_in_fc_class_p (stmt, field_class))
		read = true;

	      srfield *srf = write_field_in_fc_class_p (stmt, field_class);
	      if (!srf)
		continue;

	      /* Multiple rhs is not considered conservatively.  */
	      if (gimple_assign_rhs_class (stmt) != GIMPLE_SINGLE_RHS)
		return false;

	      /* Initialize the read flag when recording first stmt.
		 The read flag must be false when recording
		 the rest of the stmts.  */
	      if (group.is_empty ())
		read = false;
	      else if (read)
		return false;

	      int index = field_class->get_field_index (srf);
	      if (index == -1 || !bitmap_set_bit (visited_fields, index))
		return false;

	      indexes.safe_push (index);
	      group.safe_push (stmt);
	      if (group.length () == field_class->size ())
		{
		  if (!fc_pair_stmts_rhs_equal_p (group))
		    return false;

		  add_pair_stmts_group (info, group, indexes);
		  info.pair_stmts_func.safe_push (srfn);
		  group.truncate (0);
		  indexes.truncate (0);
		  bitmap_clear (visited_fields);
		}

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "[BB #%d] Record stmt: ", bb->index);
		  print_gimple_stmt (dump_file, stmt, 0);
		}
	    }

	  /* Only support one unpair assignment now.  */
	  if (!group.is_empty ())
	    {
	      if (info.unpair_stmt || group.length () > 1)
		return false;
	      info.unpair_stmt = group[0];
	      info.unpair_stmt_func = srfn;
	      info.unpair_stmt_index = indexes[0];
	    }
	}
    }

  return true;
}

/* Reorder a group of pair stmts and add it to fc_shadow_info.  */

void
ipa_struct_reorg::add_pair_stmts_group (fc_shadow_info &info,
					const auto_vec<gimple *> &group,
					const auto_vec<unsigned> &indexes)
{
  auto ordered_group = new auto_vec<gimple *> (group.length ());

  unsigned i;
  unsigned ordered_index;
  FOR_EACH_VEC_ELT (indexes, i, ordered_index)
    (*ordered_group)[ordered_index] = group[i];

  info.pair_stmts_groups.safe_push (ordered_group);
}

/* Check if the stmt reads any field in the given field class.  */

srfield *
ipa_struct_reorg::read_field_in_fc_class_p (gimple *stmt,
					    fc_field_class *fclass)
{
  for (unsigned i = 1; i < gimple_num_ops (stmt); i++)
    {
      tree base = NULL_TREE;
      srtype *type = NULL;
      srfield *field = NULL;
      if (!get_base_type (gimple_op (stmt, i), base, type, field))
	continue;

      if (field && field->field_class == fclass)
	return field;
    }

  return NULL;
}

/* Check if the stmt reads any field in the given field class.  */

srfield *
ipa_struct_reorg::write_field_in_fc_class_p (gimple *stmt,
					     fc_field_class *fclass)
{
  tree base = NULL_TREE;
  srtype *type = NULL;
  srfield *field = NULL;

  if (!get_base_type (gimple_assign_lhs (stmt), base, type, field)
      || !field
      || field->field_class != fclass)
    return NULL;

  return field;
}

/* Check if the init_const is a unique constant, which is different from all
   constant rhs of pair statements.  */

bool
ipa_struct_reorg::unique_init_const_p (const fc_shadow_info &shadow_info)
{
  tree init_const = gimple_assign_rhs1 (shadow_info.unpair_stmt);
  HOST_WIDE_INT value = tree_to_shwi (init_const);
  for (auto *stmts : shadow_info.pair_stmts_groups)
    {
      /* We have prove all rhs in a group are equal, checking one of them
	 is enough.  */
      tree rhs = gimple_assign_rhs1 ((*stmts)[0]);
      if (TREE_CODE (rhs) != INTEGER_CST)
	continue;

      if (tree_to_shwi (rhs) == value)
	{
	  FC_DUMP_MSG ("Init const is not unique.\n");
	  return false;
	}
    }

  return true;
}

/* Check if the right operands of all assignments are equal.  */

bool
ipa_struct_reorg::fc_pair_stmts_rhs_equal_p (const auto_vec<gimple *> &stmts)
{
  if (stmts.length () < 2)
    return false;

  tree rhs = gimple_assign_rhs1 (stmts[0]);
  for (unsigned i = 1; i < stmts.length (); i++)
    if (!fc_operand_equal_p (rhs, gimple_assign_rhs1 (stmts[i])))
      return false;

  return true;
}

/* Check if VAR1 and VAR2 are equal for field compression.  */

bool
ipa_struct_reorg::fc_operand_equal_p (tree var1, tree var2)
{
  if (operand_equal_p (var1, var2))
    return true;

  /* Match code and operands.  */
  tree_code code = TREE_CODE (var1);
  if (code != TREE_CODE (var2))
    return false;

  if (code == SSA_NAME)
    {
      gimple *stmt1 = SSA_NAME_DEF_STMT (var1);
      gimple *stmt2 = SSA_NAME_DEF_STMT (var2);
      return is_gimple_assign (stmt1) && is_gimple_assign (stmt2)
	     && fc_operand_equal_p (gimple_assign_rhs_to_tree (stmt1),
				    gimple_assign_rhs_to_tree (stmt2));
    }

  /* Only part of the cases are covered now.  */
  HOST_WIDE_INT value;
  switch (get_gimple_rhs_class (code))
    {
      case GIMPLE_UNARY_RHS:
	return ((code == COMPONENT_REF || code == MEM_REF)
		&& fc_global_const_p (var1, value)
		&& operand_equal_p (var1, var2, COMPARE_DECL_FLAGS));
      case GIMPLE_BINARY_RHS:
	return fc_operand_equal_p (TREE_OPERAND (var1, 0),
				   TREE_OPERAND (var2, 0))
	       && fc_operand_equal_p (TREE_OPERAND (var1, 1),
				      TREE_OPERAND (var2, 1));
      default:
	return false;
    }
}

/* Return true if VAR is a global variable, and it is assigned to be a constant
   and it is never changed globally.  The assumption is this VAR doesn't have
   address taken, and the type containing it doesn't escape.  */

bool
ipa_struct_reorg::fc_global_const_p (tree var, HOST_WIDE_INT &value)
{
  const_map *cm = find_global_const (var);
  if (cm)
    {
      value = cm->value;
      return true;
    }

  if (visited_vars.contains (var))
    return false;
  visited_vars.add (var);

  bool is_const = false;
  HOST_WIDE_INT const_value = 0;
  for (auto *srfn : functions)
    {
      SET_CFUN (srfn);
      basic_block bb = NULL;
      FOR_EACH_BB_FN (bb, cfun)
	{
	  for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	       gsi_next (&si))
	    {
	      gimple *stmt = gsi_stmt (si);
	      if (!gimple_assign_single_p (stmt)
		  || !operand_equal_p (gimple_assign_lhs (stmt), var))
		continue;

	      if (!fc_peephole_const_p (gimple_assign_rhs1 (stmt), value))
		return false;

	      /* Make sure the value is never changed.  */
	      if (is_const)
		{
		  if (value != const_value)
		    return false;
		  continue;
		}

	      is_const = true;
	      const_value = value;

	      /* Record a global constant here.  */
	      global_consts.safe_push (new const_map (var, value));
	    }
	}
    }

  return is_const;
}

/* Return true if VAR is a simple constant that can be identified by peephole.
   and the HWI will be updated accordingly.  Otherwise, the HWI will not be
   changed.  */

bool
ipa_struct_reorg::fc_peephole_const_p (tree var, HOST_WIDE_INT &value)
{
  if (TREE_CODE (var) == INTEGER_CST)
    {
      value = tree_to_shwi (var);
      return true;
    }

  if (TREE_CODE (var) != SSA_NAME)
    return false;

  /* Var might be an argument.  */
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  if (!is_gimple_assign (stmt))
    return false;

  if (gimple_assign_load_p (stmt))
    return fc_global_const_p (gimple_assign_rhs1 (stmt), value);

  HOST_WIDE_INT value1, value2;
  if (gimple_assign_rhs_class (stmt) != GIMPLE_BINARY_RHS
      || !fc_peephole_const_p (gimple_assign_rhs1 (stmt), value1)
      || !fc_peephole_const_p (gimple_assign_rhs2 (stmt), value2))
    return false;

  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  switch (rhs_code)
    {
      case PLUS_EXPR:
	value = value1 + value2;
	break;
      case MULT_EXPR:
	value = value1 * value2;
	break;
      case MAX_EXPR:
	value = (value1 > value2) ? value1 : value2;
	break;
      case MIN_EXPR:
	value = (value1 < value2) ? value1 : value2;
	break;
      default:
	return false;
    }

  return true;
}

const_map *
ipa_struct_reorg::find_global_const (tree var)
{
  for (auto *cm : global_consts)
    if (operand_equal_p (cm->var, var))
      return cm;

  return NULL;
}

/* The unpair statement need to meet the following requirements:
   (1) The array being accessed(mem base) should be allocated by calloc()
       so that we can make sure its values are initialized as zero
   (2) There must not be any assignment to other fields in the same
       field class in the same array before the unpair stmt

   These requirements are to ensure we know the value of shadow fields
   when an unpair stmt happens.  */

bool
ipa_struct_reorg::check_unpair_stmt (fc_field_class *field_class,
				     gimple *unpair_stmt,
				     srfunction *unpair_stmt_func,
				     srfield *unpair_field)
{
  SET_CFUN (unpair_stmt_func);

  srtype *type = NULL;
  srfield *field = NULL;
  tree base = NULL_TREE;
  if (!get_base_type (gimple_assign_lhs (unpair_stmt), base, type, field))
    return false;

  /* The array being accessed.  */
  tree mem_base = find_mem_base (base);
  if (!mem_base)
    return false;

  auto blocks = collect_all_predecessor (unpair_stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Found %d blocks that can reach <bb %d> in func ",
	       blocks.length (), gimple_bb (unpair_stmt)->index);
      print_generic_expr (dump_file, cfun->decl);
      fprintf (dump_file, ":\n");

      for (auto *bb : blocks)
	fprintf (dump_file, "%d ", bb->index);

      fprintf (dump_file, "\n\n");
    }

  /* Check requirement (2).  */
  for (auto *bb : blocks)
    {
      for (auto gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (!is_gimple_assign (stmt))
	    continue;

	  if (!get_base_type (gimple_assign_lhs (stmt), base, type, field)
	      || field->field_class != field_class
	      || field == unpair_field)
	    continue;

	  /* Accessing to different array is allowed.  */
	  tree cur_mem_base = find_mem_base (base);
	  if (!cur_mem_base || operand_equal_p (mem_base, cur_mem_base))
	    return false;
	}
    }

  return true;
}

/* Search backward following def/use chain until finding a memory pointer.  */

tree
ipa_struct_reorg::find_mem_base (tree var)
{
  auto_bitmap visited;
  auto_vec<tree> worklists;
  worklists.safe_push (var);

  tree mem_base = NULL_TREE;
  while (!worklists.is_empty ())
    {
      tree t = worklists.pop ();
      if (TREE_CODE (t) != SSA_NAME)
	{
	  gimple *alloc_stmt = find_alloc_stmt (t);
	  if (!alloc_stmt || !is_gimple_call (alloc_stmt))
	    return NULL;

	  tree alloc_lhs = gimple_call_lhs (alloc_stmt);
	  if (mem_base && !operand_equal_p (mem_base, alloc_lhs))
	    return NULL;

	  mem_base = alloc_lhs;
	  continue;
	}

      if (!bitmap_set_bit (visited, SSA_NAME_VERSION (t)))
	continue;

      gimple *stmt = SSA_NAME_DEF_STMT (t);
      if (gimple_call_builtin_p (stmt, BUILT_IN_CALLOC))
	{
	  if (mem_base && !operand_equal_p (mem_base, t))
	    return NULL;
	  mem_base = t;
	}
      else if (gimple_assign_rhs_code_p (stmt, POINTER_PLUS_EXPR)
	       || gimple_assign_single_p (stmt))
	{
	  worklists.safe_push (gimple_assign_rhs1 (stmt));
	}
      else if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
	    worklists.safe_push (gimple_phi_arg_def (stmt, i));
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (mem_base)
	{
	  FC_DUMP_MSG ("Found memory base: ");
	  print_generic_expr (dump_file, mem_base);
	  fprintf (dump_file, "\n");
	}
      else
	FC_DUMP_MSG ("Fail finding memory base\n");
    }

  return mem_base;
}

/* Return allocation stmt for a non-ssa var.

   _1 = calloc(...);
   var = _1;

   We will try to find the above pattern and return the first stmt.  */

gimple *
ipa_struct_reorg::find_alloc_stmt (tree var)
{
  basic_block bb = NULL;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  if (!is_gimple_assign (stmt)
	      || !operand_equal_p (gimple_assign_lhs (stmt), var))
	    continue;

	  if (!gimple_assign_single_p (stmt))
	    return NULL;

	  tree rhs1 = gimple_assign_rhs1 (stmt);
	  if (integer_zerop (rhs1))
	    continue;

	  if (TREE_CODE (rhs1) != SSA_NAME)
	    return NULL;

	  gimple *def_stmt = SSA_NAME_DEF_STMT (rhs1);
	  if (!gimple_call_builtin_p (def_stmt, BUILT_IN_CALLOC))
	    return NULL;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      FC_DUMP_MSG ("Found allocation stmt: ");
	      print_gimple_stmt (dump_file, def_stmt, 0);
	    }

	  return def_stmt;
	}
    }

  return NULL;
}

/* Scan field's assignments globally to determine whether it can be statically
   compressed or not.  */

bool
ipa_struct_reorg::static_compress_p (fc_type_info *info, tree field)
{
  HOST_WIDE_INT max_value = find_max_value (info->type, field);

  /* We cannot know the max value at compile time, if it is 0.  */
  if (!max_value)
    return false;

  fc_field *fc_f = new fc_field (field, max_value, 0);
  info->static_fc_fields.safe_push (fc_f);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Found a static compression field: ");
      print_generic_expr (dump_file, field);
      fprintf (dump_file, ", max_value = %ld\n", max_value);
    }

  return true;
}

/* Scan field's assignments globally to find the max value.  */

HOST_WIDE_INT
ipa_struct_reorg::find_max_value (srtype *type, tree field)
{
  auto_vec<tree> worklist;
  auto_bitmap visited;
  HOST_WIDE_INT max_value = 0;

  for (auto *access : type->accesses)
    {
      if (!access->write_field_p (field)
	  || !gimple_assign_single_p (access->stmt))
	continue;

      SET_CFUN (access->function);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Check stmt: ");
	  print_gimple_stmt (dump_file, access->stmt, 0);
	  fprintf (dump_file, "\n");
	}

      auto [found, value] = find_assign_max_value (access->stmt);
      if (!found)
	return 0;

      if (value > UINT_MAX)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      FC_DUMP_MSG ("Max value of ");
	      print_generic_expr (dump_file, field);
	      fprintf (dump_file, " is too big, max_value = %ld\n", value);
	    }
	  return 0;
	}

      if (value > max_value)
	max_value = value;
    }

  return max_value;
}

/* Trace back to find the max value.
   Return value contains two parts:
     first:
       true: Found a max value.
       false: Otherwise.
     second: The max value if found.  */

std::pair<bool, HOST_WIDE_INT>
ipa_struct_reorg::find_assign_max_value (gimple *stmt)
{
  auto_vec<tree> worklist;
  auto_bitmap visited;
  HOST_WIDE_INT max_value = 0;

  worklist.safe_push (gimple_assign_rhs1 (stmt));
  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();

      if (TREE_CODE (t) == INTEGER_CST)
	{
	  HOST_WIDE_INT value = TREE_INT_CST_LOW (t);
	  if (value < 0)
	    return {false, 0};
	  if (value > max_value)
	    max_value = value;
	  continue;
	}

      /* Trace back through ssa's def chain.  */
      if (TREE_CODE (t) != SSA_NAME)
	return {false, 0};

      if (!bitmap_set_bit (visited, SSA_NAME_VERSION (t)))
	continue;

      gimple *def_stmt = SSA_NAME_DEF_STMT (t);
      if (gimple_code (def_stmt) == GIMPLE_PHI)
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (def_stmt); i++)
	    worklist.safe_push (gimple_phi_arg_def (def_stmt, i));
	}
      else if (gimple_code (def_stmt) == GIMPLE_ASSIGN)
	{
	  if (gimple_assign_rhs_class (def_stmt) != GIMPLE_SINGLE_RHS
	      && !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt)))
	    return {false, 0};
	  worklist.safe_push (gimple_assign_rhs1 (def_stmt));
	}
      else
	{
	  return {false, 0};
	}
    }

  return {true, max_value};
}

/* Check if it is a struct copy.  */

bool
ipa_struct_reorg::struct_copy_p (gimple *stmt, tree type)
{
  if (!gimple_assign_single_p (stmt)
      || TREE_TYPE (gimple_assign_lhs (stmt)) != type
      || !types_fc_compatible_p (TREE_TYPE (gimple_assign_rhs1 (stmt)), type))
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Found struct copy: \n");
      print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "\n");
    }

  return true;
}

/* Return true if we can find a hot loop, in which
   (1) there is a struct copy for the fc_type
   (2) or, all fc_fields have been written.  */

bool
ipa_struct_reorg::find_hot_access (fc_type_info *info,
				   auto_vec<fc_field *> &fc_fields)
{
  /* Record which fields are written in a block.  */
  hash_map<basic_block, hash_set<tree>> write_map;

  srtype *type = info->type;
  for (auto *access : type->accesses)
    {
      SET_CFUN (access->function);

      basic_block bb = access->stmt->bb;
      if (!bb->loop_father->num || !access->write_p ())
	continue;

      /* Case (1).  */
      if (struct_copy_p (access->stmt, type->type))
	return true;

      /* Case (2).  */
      if (!access->field)
	continue;

      tree fielddecl = access->field->fielddecl;
      if (!fielddecl || !find_fc_field (fc_fields, fielddecl))
	continue;

      auto &set = write_map.get_or_insert (bb);
      set.add (fielddecl);

      /* Now all fields have been written.  */
      if (set.elements () == fc_fields.length ())
	return true;
    }

  return false;
}

/* Clean up all of write stmts to shadow field by changing the RHS to be true,
   which means it is a shadow.  */

void
ipa_struct_reorg::cleanup_shadow_write (fc_type_info *info)
{
  for (auto *fc_f : info->static_fc_fields)
    {
      if (!fc_f->original)
	continue;

      unsigned i;
      gimple *stmt;
      FOR_EACH_VEC_ELT (fc_f->shadow_stmts, i, stmt)
	{
	  SET_CFUN (fc_f->shadow_stmts_func[i]);
	  gcc_assert (gimple_assign_single_p (stmt));
	  tree newrhs = build_int_cst (TREE_TYPE (fc_f->field), 1);
	  gimple_assign_set_rhs1 (stmt, newrhs);
	  update_stmt (stmt);
	}
    }
}

/* Rewrite all of read of shadow field by using question expression.  */

void
ipa_struct_reorg::rewrite_shadow_read (fc_type_info *info)
{
  for (auto *fc_f : info->static_fc_fields)
    {
      if (!fc_f->original)
	continue;

      for (auto *access : info->type->accesses)
	{
	  if (!access->read_field_p (fc_f->field))
	    continue;

	  SET_CFUN (access->function);
	  modify_shadow_read (access->stmt, access->index,
			      fc_f, access->base);
	}
    }
}

/* Insert the followings for shadow data read before STMT.
   The IDX operand is the shadow data.

   * For static:  (shadow_field == true) ? original_field : 0
   * For dynamic: (original_field != init_const) ? original_field : 0
 */

void
ipa_struct_reorg::modify_shadow_read (gimple *stmt, unsigned idx,
				      fc_field *field, tree base)
{
  tree shadow = gimple_op (stmt, idx);
  tree original = build_field_ref (base, field->original->fielddecl);

  /* Insert new stmt immediately before stmt.  */
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);

  /* original_ssa = original */
  tree original_ssa = make_temp_ssa_name (TREE_TYPE (original), NULL, "");
  gimple *original_stmt = gimple_build_assign (original_ssa, original);
  gsi_insert_before (&gsi, original_stmt, GSI_SAME_STMT);
  update_stmt (original_stmt);

  tree cond = NULL_TREE;
  if (current_fc_level == fc_level::DYNAMIC)
    {
      field->original->get_closure ()->add_read_change (original_stmt);
      /* new_shadow_ssa = (original_ssa != init_const ? original_ssa : 0) */
      cond = fold_build2 (NE_EXPR, boolean_type_node, original_ssa,
			  field->init_const);
    }
  else
    {
      /* shadow_ssa = shadow */
      tree shadow_ssa = make_temp_ssa_name (TREE_TYPE (shadow), NULL, "");
      gimple *shadow_stmt = gimple_build_assign (shadow_ssa, shadow);
      gsi_insert_before (&gsi, shadow_stmt, GSI_SAME_STMT);
      update_stmt (shadow_stmt);

      /* new_shadow_ssa = (shadow_ssa == true ? original_ssa : 0) */
      cond = fold_build2 (EQ_EXPR, boolean_type_node, shadow_ssa,
			  build_int_cst (TREE_TYPE (shadow), 1));
    }

  tree new_shadow = build_cond_expr (cond, original_ssa,
				     build_int_cst (TREE_TYPE (shadow), 0));
  new_shadow = force_gimple_operand_gsi (&gsi, new_shadow, true, NULL, true,
					 GSI_SAME_STMT);
  tree new_shadow_ssa = make_temp_ssa_name (TREE_TYPE (shadow), NULL, "");
  gimple *new_shadow_stmt = gimple_build_assign (new_shadow_ssa, new_shadow);
  gsi_insert_before (&gsi, new_shadow_stmt, GSI_SAME_STMT);

  gimple_set_op (stmt, idx, new_shadow_ssa);
  update_stmt (new_shadow_stmt);
  update_stmt (stmt);
}

/* Compress fields and create static new field types.  */

bool
ipa_struct_reorg::compress_fields_static (fc_type_info *info)
{
  /* For static compression fields, compress them according to max_value.  */
  for (auto *fc_f : info->static_fc_fields)
    {
      tree old_type = TREE_TYPE (fc_f->field);
      tree new_type = NULL_TREE;

      HOST_WIDE_INT max_value = fc_f->max_value;
      gcc_assert (max_value > 0 && max_value <= UINT_MAX);

      /* Conservatively we only do static compression for unsigned type.  */
      if (max_value <= 0xff)
	new_type = unsigned_char_type_node;
      else if (max_value <= 0xffff)
	new_type = short_unsigned_type_node;
      else
	new_type = unsigned_type_node;

      fc_f->new_type = new_type;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Change the type of ");
	  print_generic_expr (dump_file, fc_f->field);
	  fprintf (dump_file, " from (prec=%d) to ", TYPE_PRECISION (old_type));
	  print_generic_expr (dump_file, new_type);
	  fprintf (dump_file, "(prec=%d)\n", TYPE_PRECISION (new_type));
	}
    }

  return true;
}

/* Compress fields to bitfield, for which bits will be the width.  */

bool
ipa_struct_reorg::compress_to_bitfield_static (fc_type_info *info)
{
  if (!flag_ipa_struct_sfc_bitfield)
    return true;

  for (auto *fc_f : info->static_fc_fields)
    {
      HOST_WIDE_INT max_value = fc_f->max_value;
      gcc_assert (max_value > 0 && max_value <= UINT_MAX);

      /* Calculate bitsize.  */
      fc_f->bits = 0;
      while (max_value)
	{
	  fc_f->bits++;
	  max_value >>= 1;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Static bitfield: ");
	  print_generic_expr (dump_file, fc_f->field);
	  fprintf (dump_file, ":%d", fc_f->bits);
	  fprintf (dump_file, "\n");
	}
    }

  return true;
}

/* Compress fields to bitfield for dynamic field compression.  */

bool
ipa_struct_reorg::compress_to_bitfield_dynamic (fc_type_info *info)
{
  if (!flag_ipa_struct_dfc_bitfield)
    return true;

  calc_fc_ref_count (info);

  /* Collect existing bitfields.  */
  unsigned total_static_bits = 0;
  for (auto *srf : info->type->fields)
    {
      tree field = srf->fielddecl;
      if (DECL_BIT_FIELD (field))
	total_static_bits += tree_to_uhwi (DECL_SIZE (field));
    }

  unsigned max_ref_cnt = 0;
  fc_field *max_f = NULL;
  fc_cond *max_cond = NULL;
  for (auto *cond : info->fc_conds)
    {
      /* Heuristically, only try bit field for big data size.  */
      if (TYPE_MAIN_VARIANT (cond->old_type) != long_integer_type_node)
	continue;

      /* Find the hottest field.  */
      for (auto *fc_f : cond->fields)
	{
	  if (fc_f->ref_cnt <= max_ref_cnt)
	    continue;

	  max_ref_cnt = fc_f->ref_cnt;
	  max_f = fc_f;
	  max_cond = cond;
	}
    }

  /* Choose the hottest candidate to try bitfield.  */
  unsigned new_type_bits = TYPE_PRECISION (max_f->new_type);
  if (new_type_bits <= total_static_bits)
    return false;

  /* The fc condition covering this field is marked as bitfield,
     although not all of the fields for this condition are marked as
     bitfield.  */
  max_f->bits = new_type_bits - total_static_bits;
  max_cond->bits = max_f->bits;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Dynamic bitfield: ");
      print_generic_expr (dump_file, max_f->field);
      fprintf (dump_file, ":%d", max_f->bits);
      fprintf (dump_file, "\n");
    }

  return true;
}

/* Collect all blocks that can reach stmt.  */

auto_vec<basic_block>
ipa_struct_reorg::collect_all_predecessor (gimple *stmt)
{
  auto_vec<basic_block> blocks;
  basic_block start_bb = gimple_bb (stmt);

  if (start_bb)
    {
      auto_bitmap visited;
      auto_vec<basic_block> worklists;
      worklists.safe_push (start_bb);

      while (!worklists.is_empty ())
	{
	  basic_block bb = worklists.pop ();
	  if (!bitmap_set_bit (visited, bb->index))
	    continue;

	  blocks.safe_push (bb);
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    worklists.safe_push (e->src);
	}
    }

  return blocks;
}

bool
ipa_struct_reorg::types_fc_equal_p (tree type1, tree type2)
{
  if (type1 == type2)
    return true;
  if (TREE_CODE (type1) != TREE_CODE (type2))
    return false;

  const char *tname1;
  const char *tname2;
  size_t len1;
  size_t len2;
  const char *p;

  switch (TREE_CODE (type1))
    {
      case POINTER_TYPE:
	return types_fc_equal_p (inner_type (type1), inner_type (type2));

      case RECORD_TYPE:
	tname1 = get_type_name (type1);
	tname2 = get_type_name (type2);
	if (!tname1 || !tname2)
	  return false;

	len1 = strlen (tname1);
	len2 = strlen (tname2);
	if (len1 > len2)
	  {
	    std::swap (len1, len2);
	    std::swap (tname1, tname2);
	  }

	p = strstr (tname2, tname1);
	if (!p)
	  return false;
	p += len1;

	/* As suffixes with '.' are generated by compiler, should be safe to
	   skip the rest of p.  */
	return STRING_STARTS_WITH (p, ".reorg");

      default:
	return false;
    }
}

bool
ipa_struct_reorg::types_fc_compatible_p (tree type1, tree type2)
{
  return types_compatible_p (type1, type2) || types_fc_equal_p (type1, type2);
}

/* Scan all of fields to check whether each can be dynamically
   compressed or not.  */

bool
ipa_struct_reorg::find_dynamic_fc_fields (fc_type_info *info)
{
  if (flag_ipa_struct_dfc_shadow)
    find_shadow_fields (info);

  if (!find_fields_in_input_stmt (info))
    {
      FC_DUMP_MSG ("Fail finding fields in input stmt\n");
      return false;
    }

  if (!find_fopen_fclose (info))
    {
      FC_DUMP_MSG ("Fail finding fopen/fclose stmt\n");
      return false;
    }

  /* Avoid compressing fields without hot access.  */
  if (!find_hot_access (info, info->dynamic_fc_fields))
    {
      FC_DUMP_MSG ("Fail finding hot access for dynamic\n");
      return false;
    }

  if (!check_dynamic_shadow_fields (info))
    {
      FC_DUMP_MSG ("Fail checking dynamic shadow fields\n");
      return false;
    }

  if (!find_fc_paths (info))
    {
      FC_DUMP_MSG ("Fail finding fc paths\n");
      return false;
    }

  if (!find_fc_data (info))
    {
      FC_DUMP_MSG ("Fail finding fc data\n");
      return false;
    }

  return true;
}

/* Find the stmt that read data from a file, the fields that can be affected
   by the input data will be treated as the dynamic field compression
   candidate fields.  */

bool
ipa_struct_reorg::find_fields_in_input_stmt (fc_type_info *info)
{
  basic_block input_bb = NULL;

  for (auto *access : info->type->accesses)
    {
      if (!access->write_field_p ())
	continue;

      srfield *field = access->field;
      if (find_fc_field (info->dynamic_shadow_fields, field->fielddecl)
	  || find_fc_field (info->dynamic_fc_fields, field->fielddecl))
	continue;

      /* Skip dead field.  */
      if (field->dead_field_p ())
	continue;

      if (TREE_CODE (field->fieldtype) != INTEGER_TYPE)
	continue;

      SET_CFUN (access->function);
      /* Guarantee this struct field is from a file.  */
      gimple *input_stmt = NULL;
      gimple *var_stmt = NULL;
      if (!find_input_stmt (access->stmt, input_stmt, var_stmt)
	  || gimple_bb (input_stmt)->loop_father->num == 0)
	continue;

      tree var = gimple_assign_rhs1 (var_stmt);

      if (!info->input_stmt)
	{
	  info->input_stmt = input_stmt;
	  info->input_var = access->base;
	  info->input_file_handler = find_file_handler (input_stmt);
	  if (!info->input_file_handler)
	    return false;
	}

      /* Support only one input stmt now.  */
      if (info->input_stmt != input_stmt
	  || info->input_var != access->base)
	return false;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Found a dynamic compression field: ");
	  print_generic_expr (dump_file, field->fielddecl);
	  fprintf (dump_file, ", input var: ");
	  print_generic_expr (dump_file, var);
	  fprintf (dump_file, "\n");
	}

      tree in_ssa = gimple_assign_rhs1 (access->stmt);
      fc_field *fc_f = new fc_field (field->fielddecl, var, in_ssa);
      info->dynamic_fc_fields.safe_push (fc_f);

      /* All fc fields should define their ssas in the same block.  */
      if (!input_bb)
	input_bb = gimple_bb (SSA_NAME_DEF_STMT (in_ssa));
      else if (input_bb != gimple_bb (SSA_NAME_DEF_STMT (in_ssa)))
	return false;

      info->start_srfn = access->function;
    }

  if (info->dynamic_fc_fields.is_empty ())
    return false;

  /* Sort all fields in the order of input ssa position.  This is required
     to simplify the min_val and max_val calculation.  */
  SET_CFUN (info->start_srfn);
  renumber_gimple_stmt_uids_in_blocks (&input_bb, 1);
  info->dynamic_fc_fields.qsort (input_order_cmp);

  return true;
}

/* Find the input stmt for the rhs of the given stmt.
   Now we only support sscanf.  */

bool
ipa_struct_reorg::find_input_stmt (gimple *stmt, gimple *&input_stmt,
				   gimple *&var_stmt)
{
  /* Check pattern fc_type->field = _ssa_name.  */
  if (!gimple_assign_single_p (stmt)
      || TREE_CODE (gimple_assign_rhs1 (stmt)) != SSA_NAME)
    return false;

  stmt = strip_copy_stmts (SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt)));
  /* Check pattern _ssa_name = var.  */
  if (!gimple_assign_single_p (stmt)
      || !VAR_P (gimple_assign_rhs1 (stmt)))
    return false;

  var_stmt = stmt;
  tree var = gimple_assign_rhs1 (stmt);

  /* Search backward to find a sscanf stmt.  */
  while (gimple_bb (stmt))
    {
      tree vuse = gimple_vuse (stmt);
      if (!vuse)
	break;

      stmt = SSA_NAME_DEF_STMT (vuse);
      if (!gimple_call_builtin_p (stmt, BUILT_IN_SSCANF))
	continue;

      /* Search '&var' from the 3th arguments.  */
      for (unsigned i = 2; i < gimple_call_num_args (stmt); i++)
	{
	  tree arg = gimple_call_arg (stmt, i);
	  if (TREE_CODE (arg) != ADDR_EXPR
	      || TREE_OPERAND (arg, 0) != var)
	    continue;

	  input_stmt = stmt;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      FC_DUMP_MSG ("Found input stmt: ");
	      print_gimple_stmt (dump_file, stmt, 0);
	    }
	  return true;
	}

      /* The sscanf stmt doesn't contain 'var', so the check failed.  */
      break;
    }

  return false;
}

/* Find the file handler, which holds the file from which the given stmt
   read data.
   Now only support sscanf.  */

tree
ipa_struct_reorg::find_file_handler (gimple *input_stmt)
{
  if (!gimple_call_builtin_p (input_stmt, BUILT_IN_SSCANF))
    return NULL_TREE;

  /* Find fgets stmt.  */
  gimple *stmt = SSA_NAME_DEF_STMT (gimple_vuse (input_stmt));
  if (gimple_code (stmt) != GIMPLE_CALL)
    return NULL_TREE;

  tree callee = gimple_call_fn (stmt);
  if (callee && TREE_CODE (callee) == OBJ_TYPE_REF)
    return NULL_TREE;

  callee = gimple_call_fndecl (stmt);
  const char *fn_name = get_func_name (callee);
  if (!fn_name || strcmp (fn_name, "fgets") != 0)
    return NULL_TREE;

  /* Check fget is using the string for sscanf.  */
  tree fget_arg0 = gimple_call_arg (stmt, 0);
  tree sscanf_arg0 = gimple_call_arg (input_stmt, 0);
  if (TREE_OPERAND (fget_arg0, 0) != TREE_OPERAND (sscanf_arg0, 0))
    return NULL_TREE;

  return gimple_call_arg (stmt, 2);
}

/* Find fclose in start function.  */

bool
ipa_struct_reorg::find_fopen_fclose (fc_type_info *info)
{
  SET_CFUN (info->start_srfn);

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (auto si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  if (!is_gimple_call (stmt))
	    continue;

	  tree decl = gimple_call_fndecl (stmt);
	  const char *callee = get_func_name (decl);
	  if (!callee || strcmp (callee, "fclose") != 0)
	    continue;

	  /* The fclose must use the same file handler as the fget,
	     which reads data for sscanf.  */
	  tree fh = gimple_call_arg (stmt, 0);
	  if (fh != info->input_file_handler)
	    continue;

	  info->fclose_stmt = stmt;
	  renumber_gimple_stmt_uids_in_blocks (&bb, 1);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\nFound fclose in function %s:\n",
		       get_func_name (info->start_srfn->node->decl));
	      print_gimple_stmt (dump_file, stmt, 0);
	    }

	  return true;
	}
    }

  return false;
}

/* Check whether the original of a dynamic shadow field is one of
   dynamic_fc_fields.  */

bool
ipa_struct_reorg::check_dynamic_shadow_fields (fc_type_info *info)
{
  for (auto *shadow_field : info->dynamic_shadow_fields)
    {
      srfield *original = shadow_field->original;
      fc_field *input_field = find_fc_field (info->dynamic_fc_fields,
					     original->fielddecl);
      if (!input_field)
	return false;

      shadow_field->input_field = input_field;
      shadow_field->input_ssa = input_field->input_ssa;
    }

  return true;
}

bool
ipa_struct_reorg::find_fc_paths (fc_type_info *info)
{
  /* Start point function.  */
  srfunction *srfn = info->start_srfn;
  gimple *start_stmt = info->fclose_stmt;

  while (srfn)
    {
      /* Already seen.  */
      if (srfn->fc_path.start_stmt)
	return srfn->fc_path.start_stmt == start_stmt;

      SET_CFUN (srfn);

      srfn->fc_path.start_stmt = start_stmt;
      if (!srfn->fc_path.collect_blocks (start_stmt, fc_path_info::PRED)
	  || !srfn->fc_path.collect_blocks (start_stmt, fc_path_info::SUCC))
	return false;

      /* Start at the entry function.  */
      if (srfn->entry_function_p ())
	return true;

      /* The current function should only be called once.  */
      cgraph_edge *edge = srfn->node->callers;
      if (!edge || edge->next_caller || !edge->call_stmt)
	return false;

      srfn = find_function (edge->caller);
      start_stmt = edge->call_stmt;
    }

  return false;
}

bool
ipa_struct_reorg::find_fc_data (fc_type_info *info)
{
  if (!find_fc_arrays (info))
    {
      FC_DUMP_MSG ("Fail finding fc arrays\n");
      return false;
    }

  if (!check_fc_array_uses (info))
    {
      FC_DUMP_MSG ("Fail checking fc array uses\n");
      return false;
    }

  if (!find_fc_refs (info))
    {
      FC_DUMP_MSG ("Fail finding fc refs\n");
      return false;
    }

  return true;
}

/* Find all arrays to be cached:
   1. Defined by malloc/calloc (before start-point)
   2. Will be used after fclose (e.g. global variables)  */

bool
ipa_struct_reorg::find_fc_arrays (fc_type_info *info)
{
  varpool_node *vnode;
  tree type = info->type->type;

  /* 1) Process all global vars, search for arrays of cached objects.  */
  FOR_EACH_VARIABLE (vnode)
    {
      tree node = vnode->decl;
      tree node_type = TREE_TYPE (node);
      /* Global object is not supported.  */
      if (types_fc_compatible_p (node_type, type))
	return false;

      switch (TREE_CODE (node_type))
	{
	  /* POINTER->RECORD(fc_type) */
	  case POINTER_TYPE:
	    if (types_fc_compatible_p (TREE_TYPE (node_type), type)
		&& !find_fc_array (info, node, vnode))
	      return false;
	    break;
	  /* RECORD->POINTER->RECORD(fc_type) */
	  case RECORD_TYPE:
	    for (tree t = TYPE_FIELDS (node_type); t; t = DECL_CHAIN (t))
	      {
		if (TREE_CODE (t) != FIELD_DECL)
		  continue;

		tree field_type = TREE_TYPE (t);
		if (TREE_CODE (field_type) == RECORD_TYPE)
		  {
		    FC_DUMP_MSG ("RECORD->RECORD->... not supported\n");
		    return false;
		  }

		if (POINTER_TYPE_P (field_type)
		    && types_fc_compatible_p (TREE_TYPE (field_type), type))
		  {
		    tree field_node = build3 (COMPONENT_REF, field_type, node,
					      t, NULL_TREE);
		    if (!find_fc_array (info, field_node, vnode))
		      return false;
		  }
		/* More safe: trace-back following VDEF/VUSE.  */
	      }
	    break;
	  case ARRAY_TYPE:
	  case UNION_TYPE:
	  case QUAL_UNION_TYPE:
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		FC_DUMP_MSG ("node_type not handled: ");
		print_generic_expr (dump_file, node_type);
		fprintf (dump_file, "\n");
	      }
	    return false;
	  default:
	    break;
	}
    }

  return !info->fc_arrays.is_empty ();
}

/* Check if node is a array to be cached, if so, record it.
   vnode contains referrring to node.  */

bool
ipa_struct_reorg::find_fc_array (fc_type_info *info, tree node,
				 varpool_node *vnode)
{
  tree size_expr = NULL_TREE;
  tree ssa_def = NULL_TREE;

  ipa_ref *ref = NULL;
  for (unsigned i = 0; vnode->iterate_referring (i, ref); i++)
    {
      /* Filter for writes to NODE.  */
      if (ref->use != IPA_REF_STORE)
	continue;
      /* Ignore assignments after start-point.  */
      if (!is_stmt_before_fclose (info, ref->stmt, ref->referring))
	continue;
      tree lhs = gimple_get_lhs (ref->stmt);
      if (!operand_equal_p (lhs, node, COMPARE_DECL_FLAGS))
	continue;

      tree new_size_expr = NULL_TREE;
      if (!get_allocate_size_iterate (info->type->type, ref->stmt,
				      new_size_expr, &ssa_def))
	return false;

      if (new_size_expr)
	{
	  if (size_expr)
	    {
	      FC_DUMP_MSG ("fc_array allocated twice before start-point\n");
	      return false;
	    }
	  size_expr = new_size_expr;

	  /* Allocation must happen at start function.  */
	  if (ref->referring != info->start_srfn->node)
	    return false;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      FC_DUMP_MSG ("Add array: ");
	      print_generic_expr (dump_file, node);
	      fprintf (dump_file, ", size: ");
	      print_generic_expr (dump_file, size_expr);
	      fprintf (dump_file, ", ssa_def: ");
	      print_generic_expr (dump_file, ssa_def);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  if (size_expr)
    {
      if (duplicative_array_p (info, ssa_def))
	return false;

      fc_array *array = new fc_array (node, size_expr, ssa_def, vnode);
      info->fc_arrays.safe_push (array);
    }

  return true;
}

/* Give the SSA_DEF of a array, check if it's duplicative.  */

bool
ipa_struct_reorg::duplicative_array_p (fc_type_info *info, tree ssa_def)
{
  for (auto *array : info->fc_arrays)
    {
      if (array->ssa_def != ssa_def)
	continue;

      FC_DUMP_MSG ("Array assigned to multiple variable\n");
      return true;
    }

  return false;
}

bool
ipa_struct_reorg::is_stmt_before_fclose (fc_type_info *info, gimple *stmt,
					 symtab_node *node)
{
  gcc_assert (info->fclose_stmt);
  srfunction *f = find_function (as_a<cgraph_node *> (node));
  gcc_assert (f);

  if (gimple_bb (stmt) == gimple_bb (info->fclose_stmt))
    return gimple_uid (stmt) < gimple_uid (info->fclose_stmt);

  /* If array allocations are outside start-point's function, we may need to
     create global vars to record the sizes.  */
  return f->fc_path.pre_bbs.contains (gimple_bb (stmt));
}

/* Check if the VAR is a global pointer created by reorg.  */

bool
ipa_struct_reorg::reorg_ptr_p (tree var)
{
  if (TREE_CODE (var) != VAR_DECL)
    return false;

  const char *decl_name = IDENTIFIER_POINTER (DECL_NAME (var));
  if (!decl_name)
    return false;

  const char *reorg_name = strstr (decl_name, ".reorg");
  if (!reorg_name)
    return false;

  return strstr (reorg_name, "_gptr");
}

/* Return number of objects of TYPE following define chain from STMT.
   If the number is not certain, set ERROR so we can abort field compression.
   If SSA_DEF is not NULL, the ssa_name of allocated ptr will be assigned to it.
 */

bool
ipa_struct_reorg::get_allocate_size_iterate (tree type, gimple *stmt,
					     tree &size, tree *ssa_def)
{
  if (!stmt)
    return false;

  switch (gimple_code (stmt))
    {
      case GIMPLE_ASSIGN:
	return get_allocate_size_assign (type, as_a<gassign *> (stmt),
					 size, ssa_def);
      case GIMPLE_CALL:
	return get_allocate_size_call (type, as_a<gcall *> (stmt),
				       size, ssa_def);
      default:
	return false;
    }
}

bool
ipa_struct_reorg::get_allocate_size_assign (tree type, gassign *stmt,
					    tree &size, tree *ssa_def)
{
  tree rhs = gimple_assign_rhs1 (stmt);
  if ((!gimple_assign_single_p (stmt) && !gimple_assign_cast_p (stmt))
      || TREE_CODE (rhs) != SSA_NAME)
    return true;

  gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);
  /* Handle the global arrays split by struct_reorg.  */
  if (reorg_ptr_p (gimple_assign_lhs (stmt)))
    return get_allocate_size_reorg_ptr (def_stmt, size);

  return get_allocate_size_iterate (type, def_stmt, size, ssa_def);
}

bool
ipa_struct_reorg::get_allocate_size_call (tree type, gcall *stmt,
					  tree &size, tree *ssa_def)
{
  tree lhs = gimple_call_lhs (stmt);
  gcc_assert (TREE_CODE (lhs) == SSA_NAME);
  if (ssa_def)
    *ssa_def = lhs;

  size = get_allocate_size (type, lhs, NULL_TREE, stmt);

  return size != NULL_TREE;
}

/* Handle the global arrays split by struct_reorg:
   1) The new array ptrs are marked with suffix "_gptr".
   2) The array ptr are calculated with form:
      _gptr0 = calloc (NUM, size_all);
      _gptr1 = _gptr0 + NUM * sizeof (TREE_TYPE (_gptr0));
      _gptr2 = _gptr1 + NUM * sizeof (TREE_TYPE (_gptr1));
      ...
 */

bool
ipa_struct_reorg::get_allocate_size_reorg_ptr (gimple *plus_stmt, tree &size)
{
  /* Check the POINTER_PLUS_EXPR.  */
  if (!is_gimple_assign (plus_stmt)
      || gimple_assign_rhs_code (plus_stmt) != POINTER_PLUS_EXPR)
    return false;

  tree rhs1 = gimple_assign_rhs1 (plus_stmt);
  tree rhs2 = gimple_assign_rhs2 (plus_stmt);
  tree prev_type = TREE_TYPE (rhs1);

  /* Check the MULT_EXPR.  */
  gcc_assert (TREE_CODE (rhs2) == SSA_NAME);
  gimple *mul_stmt = SSA_NAME_DEF_STMT (rhs2);
  if (!is_gimple_assign (mul_stmt)
      || gimple_assign_rhs_code (mul_stmt) != MULT_EXPR)
    return false;

  tree num = gimple_assign_rhs1 (mul_stmt);
  tree mul_by = gimple_assign_rhs2 (mul_stmt);
  if (TREE_CODE (mul_by) == SSA_NAME)
    std::swap (num, mul_by);

  if (TREE_CODE (num) != SSA_NAME || TREE_CODE (mul_by) != INTEGER_CST
      || !operand_equal_p (mul_by, TYPE_SIZE_UNIT (prev_type)))
    return false;

  /* We can trace to original calloc/malloc to make this safer.  */

  size = num;

  return true;
}

/* Returns the allocated size / T size for STMT.  That is the number of
   elements in the array allocated.  */

tree
ipa_struct_reorg::get_allocate_size (tree type, tree decl, tree orig_type,
				     gimple *stmt)
{
  if (!stmt
      || gimple_code (stmt) != GIMPLE_CALL
      || !handled_allocation_stmt (stmt))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nNot an allocate statement:\n");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      return NULL_TREE;
    }

  tree struct_size = TYPE_SIZE_UNIT (type);

  /* Specify the correct size to relax multi-layer pointer.  */
  if (TREE_CODE (decl) == SSA_NAME && orig_type && isptrptr (orig_type))
    struct_size = TYPE_SIZE_UNIT (orig_type);

  tree size = gimple_call_arg (stmt, 0);

  if (gimple_call_builtin_p (stmt, BUILT_IN_REALLOC)
      || gimple_call_builtin_p (stmt, BUILT_IN_ALIGNED_ALLOC))
    size = gimple_call_arg (stmt, 1);
  else if (gimple_call_builtin_p (stmt, BUILT_IN_CALLOC))
    {
      tree arg1;
      arg1 = gimple_call_arg (stmt, 1);
      /* Check that second argument is a constant equal to
	 the size of structure.  */
      if (operand_equal_p (arg1, struct_size, 0))
	return size;
      /* ??? Check that first argument is a constant
	 equal to the size of structure.  */
      /* If the allocated number is equal to the value of struct_size,
	 the value of arg1 is changed to the allocated number.  */
      if (operand_equal_p (size, struct_size, 0))
	return arg1;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\ncalloc the correct size:\n");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      return NULL_TREE;
    }

  tree num;
  if (!is_result_of_mult (size, &num, struct_size))
    return NULL_TREE;

  return num;
}

/* Find all fc_refs (variables/arrays to be modified according to some
   fc_array):
   - Will be used after fclose (e.g. global variables).
   - Before fclose, value or array content is assigned with references to some
     recognized fc_array. (If there are multiple fc_array variables referenced
     by one fc_ref, quit field compression.  Because we don't know how to
     modify it then.)  */

bool
ipa_struct_reorg::find_fc_refs (fc_type_info *info)
{
  /* For each fc_array, follow the use chains and search for fc_refs.  */
  for (auto *array : info->fc_arrays)
    {
      gcc_assert (array->ssa_def);
      SET_CFUN (info->start_srfn);
      if (!find_fc_refs_iterate (info, array, array->ssa_def, true))
	return false;

      ipa_ref *ref = NULL;
      for (unsigned i = 0; array->vnode->iterate_referring (i, ref); i++)
	{
	  /* Filter for memory loads.  */
	  if (ref->use != IPA_REF_LOAD)
	    continue;
	  /* Ignore assignments after start-point.  */
	  if (!is_stmt_before_fclose (info, ref->stmt, ref->referring))
	    continue;
	  if (!gimple_assign_single_p (ref->stmt))
	    return false;
	  tree rhs = gimple_assign_rhs1 (ref->stmt);
	  if (!operand_equal_p (rhs, array->var, COMPARE_DECL_FLAGS))
	    continue;

	  SET_CFUN (find_function (as_a<cgraph_node *> (ref->referring)));
	  tree lhs = gimple_assign_lhs (ref->stmt);
	  if (!find_fc_refs_iterate (info, array, lhs, true))
	    return false;
	}
    }

  return true;
}

/* Given a fc_array ARRAY and a variable VAR referring to ARRAY,
   find fc_refs iteratively follow the use chain of VAR.  */

bool
ipa_struct_reorg::find_fc_refs_iterate (fc_type_info *info, fc_array *array,
					tree var, bool loop_back)
{
  switch (TREE_CODE (var))
    {
      /* 1) For SSA_NAME, iterate through use chain.  */
      case SSA_NAME:
	return find_fc_refs_ssa_name (info, array, var, loop_back);

      /* 2) For VAR_DECL, submit a fc_ref.  */
      case VAR_DECL:
	return add_fc_ref (info, array, var, NULL_TREE);

      /* 3) For MEM_REF, find fc_ref following base's def chain.  */
      case MEM_REF:
	return find_fc_refs_mem_ref (info, array, var);

      case COMPONENT_REF:
	return find_fc_refs_component_ref (info, array, var);

      default:
	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    FC_DUMP_MSG ("Unknown use kind, code: %s, var: ",
			 get_tree_code_name (TREE_CODE (var)));
	    print_generic_expr (dump_file, var);
	    fprintf (dump_file, "\n");
	  }
	return false;
    }
}

/* Find all fc_refs of a SSA_NAME var through its use chain.  */

bool
ipa_struct_reorg::find_fc_refs_ssa_name (fc_type_info *info, fc_array *array,
					 tree var, bool loop_back)
{
  use_operand_p use_p;
  imm_use_iterator iter;
  FOR_EACH_IMM_USE_FAST (use_p, iter, var)
    {
      gimple *stmt = USE_STMT (use_p);
      cgraph_node *cnode = current_function->node;
      if (!is_stmt_before_fclose (info, stmt, cnode))
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (var);
	  if (!is_stmt_before_fclose (info, def_stmt, cnode))
	    continue;

	  /* If a local ptr of compressed type is defined before start-point
	     and used after start-point, quit field compression. (Otherwise we
	     need to clone and version the ptr's define statement.)  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      FC_DUMP_MSG ("Local usage not handled: ");
	      print_gimple_stmt (dump_file, stmt, 0);
	      fprintf (dump_file, "  Defined at: ");
	      print_gimple_stmt (dump_file, def_stmt, 0);
	    }

	  return false;
	}

      tree lhs = gimple_get_lhs (stmt);
      switch (gimple_code (stmt))
	{
	  case GIMPLE_ASSIGN:
	    /* Rule out: expr(var) = X.  */
	    if (walk_tree (&lhs, check_for_ssa, var, NULL)
		|| !fc_type_pointer_p (info, lhs))
	      break;
	    if (!find_fc_refs_iterate (info, array, lhs, loop_back))
	      return false;
	    break;
	  case GIMPLE_PHI:
	    {
	      /* Check if VAR is from back_edge.  */
	      bool loop_var = false;
	      gphi *phi = as_a<gphi *> (stmt);
	      for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
		{
		  if (gimple_phi_arg_def (phi, i) != var)
		    continue;
		  edge e = gimple_phi_arg_edge (phi, i);
		  if (e->flags & EDGE_DFS_BACK)
		    {
		      loop_var = true;
		      break;
		    }
		}

	      if (!loop_var)
		{
		  if (!find_fc_refs_iterate (info, array, lhs, loop_back))
		    return false;
		}
	      else if (loop_back)
		{
		  if (!find_fc_refs_iterate (info, array, lhs, false))
		    return false;
		}
	      break;
	    }
	  case GIMPLE_DEBUG:
	  case GIMPLE_COND:
	  case GIMPLE_SWITCH:
	  case GIMPLE_NOP:
	    break;
	  default:
	    /* Cannot be sure how fc_array is used, like GIMPLE_CALL?  */
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		FC_DUMP_MSG ("fc_array usage not handled: ");
		print_gimple_stmt (dump_file, stmt, 0);
	      }
	    return false;
	}
    }
  return true;
}

/* Find all fc_refs of a MEM_REF var through its base's def chain.  */

bool
ipa_struct_reorg::find_fc_refs_mem_ref (fc_type_info *info, fc_array *array,
					tree var)
{
  if (!integer_zerop (TREE_OPERAND (var, 1)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("MEM_REF offset not handled: ");
	  print_generic_expr (dump_file, var);
	  fprintf (dump_file, "\n");
	}
      return false;
    }

  if (!fc_type_pointer_p (info, var))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Type not compatible: ");
	  print_generic_expr (dump_file, TREE_TYPE (var));
	  fprintf (dump_file, "\n");
	}
      return false;
    }

  tree base = TREE_OPERAND (var, 0);
  tree ref = get_ptr_decl (base);
  if (!ref)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Failed to get array decl from: ");
	  print_generic_expr (dump_file, base);
	  fprintf (dump_file, "\n");
	}
      return false;
    }

  return add_fc_ref (info, array, ref, NULL_TREE);
}

/* Find fc_refs of a COMPONENT_REF var.  */

bool
ipa_struct_reorg::find_fc_refs_component_ref (fc_type_info *info,
					      fc_array *array, tree var)
{
  tree base = TREE_OPERAND (var, 0);

  if (TREE_CODE (base) == VAR_DECL)
    return add_fc_ref (info, array, var, NULL_TREE);
  else if (TREE_CODE (base) == MEM_REF)
    base = TREE_OPERAND (base, 0);

  tree ref = get_ptr_decl (base);
  if (!ref)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Failed to get array decl from: ");
	  print_generic_expr (dump_file, base);
	  fprintf (dump_file, "\n");
	}
      return false;
    }

  tree field = TREE_OPERAND (var, 1);
  return add_fc_ref (info, array, ref, field);
}

/* Return the top level fc_type pointer tree node.  */

bool
ipa_struct_reorg::fc_type_pointer_p (fc_type_info *info, tree t)
{
  tree type = TREE_TYPE (t);

  return POINTER_TYPE_P (type)
	 && types_fc_compatible_p (TREE_TYPE (type), info->type->type);
}

/* Add VAR as a fc_ref entry into INFO.
   1) VAR is a single pointer: fc_ref::size = NULL, fc_ref::field = NULL
   2) VAR is an array of pointers: fc_ref::size is the size of array,
				   fc_ref::field = NULL
   3) VAR is an array of records(e.g. struct {fc_type *p;}):
	fc_ref::size is the size of array, fc_ref::field is p
 */

bool
ipa_struct_reorg::add_fc_ref (fc_type_info *info, fc_array *array, tree var,
			      tree field)
{
  /* The way we're searching for fc_refs, fc_array vars will also meet the
     requirements.  Rule out them.  */
  for (auto *d : info->fc_arrays)
    if (operand_equal_p (var, d->var, COMPARE_DECL_FLAGS))
      return true;

  tree type = NULL_TREE;
  tree size_expr = NULL_TREE;

  /* Rule out duplicants.  */
  switch (check_duplicative_ref (info, array, var, field, type, size_expr))
    {
      case check_ref_result::NEW: break;
      case check_ref_result::DUPLICATIVE: return true;
      case check_ref_result::ERROR: return false;
    }

  if (!type)
    {
      type = TREE_TYPE (var);
      /* Use the "real" type for void*.  */
      if (VOID_POINTER_P (type))
	{
	  srdecl *decl = find_decl (var);
	  if (!decl || !decl->orig_type || !POINTER_TYPE_P (decl->orig_type))
	    return false;
	  type = decl->orig_type;
	}
    }

  /* If REF is an array, get the size it is allocated with.  */
  if ((!size_expr) && POINTER_TYPE_P (type)
      && !types_fc_compatible_p (TREE_TYPE (type), info->type->type))
    {
      gimple *stmt = NULL;
      if (TREE_CODE (var) == SSA_NAME)
	stmt = SSA_NAME_DEF_STMT (var);
      else
	stmt = find_def_stmt_before_fclose (info, var);

      if (!get_allocate_size_iterate (TREE_TYPE (type), stmt, size_expr))
	return false;
    }

  fc_ref *ref = new fc_ref (var, type, array, size_expr, field);
  info->fc_refs.safe_push (ref);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Add fc_ref: ");
      ref->dump (dump_file);
    }

  return true;
}

/* Check if we have found another fc_ref with the same var.  */

check_ref_result
ipa_struct_reorg::check_duplicative_ref (fc_type_info *info, fc_array *array,
					 tree var, tree field,
					 tree &type, tree &size_expr)
{
  for (auto *ref : info->fc_refs)
    {
      if (!operand_equal_p (var, ref->var, COMPARE_DECL_FLAGS))
	continue;

      /* The var refers to multiple fc_array.  */
      if (ref->source != array)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      FC_DUMP_MSG ("Variable ");
	      print_generic_expr (dump_file, var);
	      fprintf (dump_file, " referring to multiple arrays: ");
	      print_generic_expr (dump_file, ref->source->var);
	      fprintf (dump_file, " and ");
	      print_generic_expr (dump_file, array->var);
	      fprintf (dump_file, "\n");
	    }
	  return check_ref_result::ERROR;
	}

      if (ref->field)
	{
	  gcc_assert (field);
	  /* Different fields in an array of structures.  */
	  if (!operand_equal_p (field, ref->field, COMPARE_DECL_FLAGS))
	    {
	      type = ref->orig_type ? ref->orig_type : TREE_TYPE (var);
	      size_expr = ref->size;
	      continue;
	    }
	}

      return check_ref_result::DUPLICATIVE;
    }

  return check_ref_result::NEW;
}

/* Find the single defination stmt before start-point for a var.  */

gimple *
ipa_struct_reorg::find_def_stmt_before_fclose (fc_type_info *info, tree var)
{
  tree base = TREE_CODE (var) == COMPONENT_REF ? TREE_OPERAND (var, 0) : var;
  if (TREE_CODE (base) != VAR_DECL)
    return NULL;

  varpool_node *vnode = varpool_node::get (base);
  /* Local array is not handled yet.  */
  if (!vnode)
    return NULL;

  gimple *def_stmt = NULL;
  ipa_ref *ref = NULL;
  for (unsigned i = 0; vnode->iterate_referring (i, ref); i++)
    {
      if (ref->use != IPA_REF_STORE)
	continue;

      gimple *stmt = ref->stmt;
      tree lhs = gimple_get_lhs (stmt);
      if (!operand_equal_p (lhs, var, COMPARE_DECL_FLAGS)
	  || !is_stmt_before_fclose (info, stmt, ref->referring))
	continue;

      if (gimple_assign_single_p (stmt)
	  && integer_zerop (gimple_assign_rhs1 (stmt)))
	continue;

      if (def_stmt)
	{
	  FC_DUMP_MSG ("Multiple definations before start-point?\n");
	  return NULL;
	}

      def_stmt = stmt;
    }

  return def_stmt;
}

/* VAR is an ssa_name defined by some array + offset.
   1) For global variables, returns declaration of the array.
   2) For arrays locally allocated with recogized functions, returns the
      ssa_name it is assigned with.
   3) Return NULL_TREE if cannot decide.  */

tree
ipa_struct_reorg::get_ptr_decl (tree var)
{
  if (TREE_CODE (var) != SSA_NAME)
    return NULL_TREE;

  gimple *stmt = SSA_NAME_DEF_STMT (var);
  tree var_type = TREE_TYPE (var);

  if (gimple_code (stmt) == GIMPLE_ASSIGN)
    {
      gassign *assign = as_a<gassign *> (stmt);
      switch (gimple_assign_rhs_class (assign))
	{
	  case GIMPLE_BINARY_RHS:
	    {
	      if (gimple_assign_rhs_code (assign) != POINTER_PLUS_EXPR)
		return NULL_TREE;
	      tree lhs = gimple_assign_rhs1 (assign);
	      if (types_fc_compatible_p (TREE_TYPE (lhs), var_type)
		  || VOID_POINTER_P (TREE_TYPE (lhs)))
		return get_ptr_decl (lhs);
	      return NULL_TREE;
	    }

	  case GIMPLE_UNARY_RHS:
	  case GIMPLE_SINGLE_RHS:
	    {
	      tree rhs = gimple_assign_rhs1 (stmt);
	      if (TREE_CODE (rhs) == SSA_NAME)
		return get_ptr_decl (rhs);
	      else if (TREE_CODE (rhs) == VAR_DECL)
		return rhs;
	      else if (TREE_CODE (rhs) == COMPONENT_REF)
		{
		  tree base = TREE_OPERAND (rhs, 0);
		  return DECL_P (base) ? rhs : NULL_TREE;
		}
	      else
		return NULL_TREE;
	    }
	  default:
	    return NULL_TREE;
	}
    }
  else if (gimple_code (stmt) == GIMPLE_CALL)
    return handled_allocation_stmt (stmt) ? gimple_get_lhs (stmt) : NULL_TREE;
  else
    return NULL_TREE;

  /* TODO: GIMPLE_PHI can be supported (not affecting correctness).  */
}

/* Search info->input_var backward using def/use chain until finding one of
   the arrays we have found in find_fc_arrays.  */

bool
ipa_struct_reorg::check_fc_array_uses (fc_type_info *info)
{
  hash_set<tree> visited;
  auto_vec<tree> worklist;

  visited.add (info->input_var);
  worklist.safe_push (info->input_var);

  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();
      tree_code code = TREE_CODE (t);
      if (code != SSA_NAME && code != VAR_DECL)
	continue;

      for (auto *array : info->fc_arrays)
	if (t == array->ssa_def || t == array->var)
	  return true;

      /* If we reach a global variable, it must match a fc_array.  */
      if (code == VAR_DECL)
	return false;

      gimple *stmt = SSA_NAME_DEF_STMT (t);
      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (stmt); ++i)
	    {
	      tree arg = gimple_phi_arg_def (stmt, i);
	      if (!visited.add (arg))
		worklist.safe_push (arg);
	    }
	}
      else if (gimple_assign_single_p (stmt)
	       || gimple_assign_rhs_code_p (stmt, POINTER_PLUS_EXPR))
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  if (!visited.add (rhs))
	    worklist.safe_push (rhs);
	}
    }

  return false;
}

/* Calculate the reference count of all fc fields.  */

void
ipa_struct_reorg::calc_fc_ref_count (fc_type_info *info)
{
  for (auto *access : info->type->accesses)
    {
      if (!access->field)
	continue;

      fc_field *fc_f = find_fc_field (info->dynamic_fc_fields,
				      access->field->fielddecl);
      if (fc_f)
	fc_f->ref_cnt++;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Reference count:\n");
      for (auto *fc_f : info->dynamic_fc_fields)
	{
	  print_generic_expr (dump_file, fc_f->field);
	  fprintf (dump_file, " : %d\n", fc_f->ref_cnt);
	}
    }
}

/* For dynamic fields, we heuristically change data type.  */

bool
ipa_struct_reorg::compress_fields_dynamic (fc_type_info *info)
{
  const std::map<unsigned, unsigned> precision_map{
    {64, 16}, {32, 16}, {16, 8}
  };

  for (auto *fc_f : info->dynamic_fc_fields)
    {
      tree old_type = TREE_TYPE (fc_f->field);
      bool is_unsigned = TYPE_UNSIGNED (old_type);
      gcc_assert (TREE_CODE (old_type) == INTEGER_TYPE);

      auto iter = precision_map.find (TYPE_PRECISION (old_type));
      if (iter == precision_map.cend ())
	return false;

      fc_f->new_type = get_integer_type_node (iter->second, is_unsigned);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Change the type of ");
	  print_generic_expr (dump_file, fc_f->field);
	  fprintf (dump_file, " from (prec=%d) to ", TYPE_PRECISION (old_type));
	  print_generic_expr (dump_file, fc_f->new_type);
	  fprintf (dump_file, "(prec=%d)\n", TYPE_PRECISION (fc_f->new_type));
	}

      info->record_cond (fc_f);
      fc_f->cond->new_type = fc_f->new_type;
    }

    return true;
}

/* Tune the upper boundary for dynamic fields.  The data field may be
   assigned a constant that is larger than the maximum value.  For this
   case, we can reserve a special value for it, and then we can
   compress/decompress it by creating a map between this reserved value
   and the real big value.  In the meantime, we will have to reduce the
   upper boundary for this specific field.  */

bool
ipa_struct_reorg::calc_dynamic_boundary (fc_type_info *info)
{
  /* Initialize the low_bound and high_bound.  */
  for (auto *cond : info->fc_conds)
    {
      tree ssa_type = TREE_TYPE (cond->fields[0]->input_ssa);

      /* Low bound is always zero.  */
      cond->low_bound = fold_convert (ssa_type, integer_zero_node);

      /* High bound is the max value of the type.  */
      unsigned bits = cond->bits ? cond->bits
				 : TYPE_PRECISION (cond->new_type);
      unsigned max_value = wi::max_value (bits, UNSIGNED).to_uhwi ();
      cond->high_bound = build_int_cst (ssa_type, max_value);

      auto_vec<HOST_WIDE_INT> special_values;
      /* Calculate upper bound.  */
      for (auto *access : info->type->accesses)
	{
	  gimple *stmt = access->stmt;
	  if (!gimple_assign_single_p (stmt))
	    continue;

	  /* Skip if it is not an assignment to fc field.  */
	  if (!fc_cond_field_p (gimple_assign_lhs (stmt), cond))
	    continue;

	  /* Skip if it is loaded from a fc field.  */
	  tree rhs = gimple_assign_rhs1 (stmt);
	  if (fc_field_load_p (rhs, cond))
	    continue;

	  /* Skip if it is from input_ssa.  */
	  if (fc_input_ssa_p (rhs, cond))
	    continue;

	  /* Make sure the assignment is a constant.  If possible, we
	     try to find all possible costants by peephole.  */
	  HOST_WIDE_INT value;
	  if (fc_peephole_const_p (rhs, value))
	    {
	      special_values.safe_push (value);
	      cond->field_class->closure.write_special_rhs.put (rhs, value);
	      continue;
	    }
	}

      /* Execute multiple rounds cause we didn't sort the special_values. */
      while (true)
	{
	  unsigned size = cond->special_values.length ();
	  for (auto value : special_values)
	    update_high_bound (cond, value);
	  if (size == cond->special_values.length ())
	    break;
	}
    }

  return true;
}

/* Return true if the VAR is a mem reference of fc_field in the fc_cond.  */

bool
ipa_struct_reorg::fc_cond_field_p (tree var, const fc_cond *cond)
{
  if (TREE_CODE (var) != COMPONENT_REF)
    return false;

  /* Find the stmt assigning to the fc field.  */
  tree field = TREE_OPERAND (var, 1);
  return find_fc_field (cond->fields, field);
}

/* Return true if var is one of cond's input_ssa.  */

bool
ipa_struct_reorg::fc_input_ssa_p (tree var, const fc_cond *cond)
{
  if (TREE_CODE (var) != SSA_NAME)
    return false;

  for (auto *fc_f : cond->fields)
    if (fc_f->input_ssa == var)
      return true;

  return false;
}

/* Return true the VAR is loaded from another fc field.  */

bool
ipa_struct_reorg::fc_field_load_p (tree var, const fc_cond *cond)
{
  if (TREE_CODE (var) != SSA_NAME)
    return false;

  gimple *stmt = SSA_NAME_DEF_STMT (var);
  return gimple_assign_load_p (stmt)
	 && fc_cond_field_p (gimple_assign_rhs1 (stmt), cond);
}

/* Reduce the high_bound of COND by 1, if the value is larger
   than the high_bound.  */

void
ipa_struct_reorg::update_high_bound (fc_cond *cond, HOST_WIDE_INT value)
{
  HOST_WIDE_INT high_bound = tree_to_uhwi (cond->high_bound);
  if (value >= 0 && value <= high_bound)
    return;

  if (cond->special_values.contains (value))
    return;

  high_bound--;
  cond->high_bound = build_int_cst (TREE_TYPE (cond->high_bound), high_bound);
  cond->special_values.safe_push (value);
  if (dump_file && (dump_flags & TDF_DETAILS))
    FC_DUMP_MSG ("Found special value %ld, and reduce high_bound to 0x%lx\n",
		 value, high_bound);
}

/* Check all data in fc_cond refer to a closure.  */
bool
ipa_struct_reorg::check_closure (fc_type_info *info)
{
  for (auto *cond : info->fc_conds)
    {
      if (!check_closure (info, cond))
	{
	  FC_DUMP_MSG ("Fail checking closure\n");
	  return false;
	}
    }

  return true;
}

/* All write stmts could be
   (1) unchange, i.e. optimize away compress/decompress
   (2) change, i.e. use unoptimized compress

   For case (2), we may have the scenario like below,

     B = A->field;
     ... = B;
     C->field = B;

   We still can prove C->field is from A->field, so they are
   in a closure, but we must decompress A->field and compress
   C->field, because B may be used outside the clsoure, for
   which we don't care about.  */

bool
ipa_struct_reorg::check_closure (fc_type_info *info, fc_cond *cond)
{
  fc_field_class *field_class = cond->field_class;

  for (auto *access : info->type->accesses)
    {
      if (!access->write_field_p ()
	  || access->field->field_class != field_class
	  || !fc_cond_field_p (access->expr, cond))
	continue;

      SET_CFUN (access->function);

      gimple *stmt = access->stmt;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Check closure: ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}

      /* Skip if we have already analyzed this stmt.  */
      if (field_class->closure.write_unchange_p (stmt)
	  || field_class->closure.write_change_p (stmt))
	continue;

      tree rhs = gimple_assign_rhs1 (stmt);
      HOST_WIDE_INT *value = field_class->closure.write_special_rhs.get (rhs);
      if (fc_input_ssa_p (rhs, cond)
	  || (value && cond->special_values.contains (*value)))
	{
	  /* Case (2) */
	  field_class->closure.add_write_change (stmt);
	  FC_DUMP_MSG ("Need to change.\n");
	  continue;
	}
      if (value && !cond->special_values.contains (*value))
	{
	  /* Case (2) */
	  field_class->closure.add_write_unchange (stmt);
	  FC_DUMP_MSG ("No need to change.\n");
	  continue;
	}

      if (!gimple_assign_single_p (stmt)
	  || TREE_CODE (rhs) != SSA_NAME)
	return false;

      gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);
      if (gimple_assign_single_p (def_stmt))
	{
	  /* Check if RHS is from the the same fc class.  */
	  srtype *type = NULL;
	  srfield *field = NULL;
	  tree base = NULL_TREE;
	  if (get_base_type (gimple_assign_rhs1 (def_stmt), base, type, field)
	      && field->field_class == field_class)
	    {
	      if (write_field_class_only_p (info, field_class, rhs))
		{
		  /* Case (1).  */
		  field_class->closure.add_write_unchange (stmt);
		  field_class->closure.add_read_unchange (def_stmt);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      FC_DUMP_MSG ("No need to change: ");
		      print_gimple_stmt (dump_file, def_stmt, 0);
		    }
		}
	      else
		{
		  /* Case (2).  */
		  field_class->closure.add_write_change (stmt);
		  FC_DUMP_MSG ("Need to change. \n");
		}

	      continue;
	    }
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  FC_DUMP_MSG ("Check closure fail: ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}

      return false;
    }

  collect_closure_read_change (info, field_class);

  return true;
}

/* Return true if all stmts using ssa_def are to write a fc field.  */

bool
ipa_struct_reorg::write_field_class_only_p (fc_type_info *info,
					    fc_field_class *field_class,
					    tree ssa_def)
{
  imm_use_iterator imm_iter;
  gimple *stmt;
  FOR_EACH_IMM_USE_STMT (stmt, imm_iter, ssa_def)
    {
      if (gimple_code (stmt) == GIMPLE_DEBUG)
	continue;

      /* We don't know if it is PHI.  */
      if (!is_gimple_assign (stmt))
	return false;

      srtype *type = NULL;
      srfield *field = NULL;
      tree base = NULL_TREE;
      if (!get_base_type (gimple_assign_lhs (stmt), base, type, field)
	  || type != info->type
	  || !field_class->srfields.contains (field))
	return false;
    }

  return true;
}

/* Collect read_change.  */

void
ipa_struct_reorg::collect_closure_read_change (fc_type_info *info,
					       fc_field_class *field_class)
{
  for (auto *access : info->type->accesses)
    {
      if (!access->read_field_p ()
	  || access->field->field_class != field_class)
	continue;

      /* Skip statement that has been marked as unchanged.  */
      if (field_class->closure.read_unchange_p (access->stmt))
	continue;

      field_class->closure.add_read_change (access->stmt);
    }
}

unsigned
ipa_struct_reorg::execute_dynamic_field_compression ()
{
  if (current_fc_level != fc_level::DYNAMIC)
    return 0;

  current_layout_opt_level = STRUCT_REORDER_FIELDS;
  replace_type_map.empty ();
  record_accesses ();
  prune_escaped_types ();
  check_and_prune_struct_for_field_compression ();

  return dynamic_fc_rewrite ();
}

unsigned
ipa_struct_reorg::dynamic_fc_rewrite ()
{
  if (!create_dynamic_fc_newtypes ())
    {
      FC_DUMP_MSG ("Failed to create newtypes for dfc\n");
      return 0;
    }

  for (auto *info : fc_infos)
    {
      if (!info->dynamic_fc_p)
	continue;
      create_dynamic_fc_convert_fn (info);
      clone_dynamic_fc_path (info);
      record_dfc_path_info (info);
      if (flag_ipa_struct_dfc_shadow)
	rewrite_dynamic_shadow_fields (info);
      rewrite_dynamic_fc_path ();
      add_dynamic_checking (info);
    }

  return TODO_verify_all;
}

bool
ipa_struct_reorg::create_dynamic_fc_newtypes ()
{
  bool created = false;
  for (auto *info : fc_infos)
    {
      if (!info->dynamic_fc_p)
	continue;

      create_dynamic_fc_variant (info);
      if (info->type->create_new_type ())
	created = true;
      else
	info->dynamic_fc_p = false;
    }

  if (!created)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "=========== all created newtypes ===========\n\n");
      dump_newtypes (dump_file);
    }

  return true;
}

/* Create a new fc_variant for the given fc_type in terms of fc_conds.  */

void
ipa_struct_reorg::create_dynamic_fc_variant (fc_type_info *info)
{
  create_global_var_dfc_path (info);
  info->variant = new fc_variant ();
}

/* Create a global variable to identify the current dynamic path.  */

void
ipa_struct_reorg::create_global_var_dfc_path (fc_type_info *info)
{
  tree name = get_identifier ("dfc.path");
  tree var = build_decl (BUILTINS_LOCATION, VAR_DECL, name,
			 boolean_type_node);

  TREE_PUBLIC (var) = 1;
  TREE_STATIC (var) = 1;
  DECL_IGNORED_P (var) = 1;
  DECL_ARTIFICIAL (var) = 1;
  DECL_INITIAL (var) = boolean_false_node;
  SET_DECL_ASSEMBLER_NAME (var, name);

  varpool_node::finalize_decl (var);
  info->dfc_path = var;
}

/* Insert compress/decompress functions.  */

void
ipa_struct_reorg::create_dynamic_fc_convert_fn (fc_type_info *info)
{
  for (unsigned i = 0; i < info->fc_conds.length (); i++)
    {
      fc_cond *cond = info->fc_conds[i];
      cond->compress_fn = create_convert_fn (cond, i, false);
      cond->decompress_fn = create_convert_fn (cond, i, true);
    }
}

/* Create function to further compress fields with special_values.
   - DECOMP == 0: create function to compress the field.
   - DECOMP != 0: create function to decompress the field.
   IDX is an unique number for function name.
   Return declaration of created function.  */

tree
ipa_struct_reorg::create_convert_fn (fc_cond *fcond, unsigned idx,
				     bool decompress)
{
  if (fcond->special_values.is_empty ())
    return NULL_TREE;

  push_cfun (NULL);

  /* Init declarations.  */
  char fn_name[64];
  const char *name = decompress ? "dfc.decompress." : "dfc.compress.";
  sprintf (fn_name, "%s%d", name, idx);

  tree arg_type = decompress ? fcond->new_type : fcond->old_type;
  tree return_type = decompress ? fcond->old_type : fcond->new_type;
  tree fn_decl = create_new_fn_decl (fn_name, 1, &arg_type, return_type);

  basic_block return_bb = init_lowered_empty_function (
			    fn_decl, true, profile_count::uninitialized ());
  calculate_dominance_info (CDI_DOMINATORS);

  split_edge (single_pred_edge (return_bb));
  tree result = make_ssa_name (return_type);
  create_phi_node (result, return_bb);

  /* Create compress/decompress function body.  */
  edge exit_e = create_normal_part (fcond);
  create_conversion_part (fcond, exit_e, decompress);

  /* Return stmt.  */
  update_stmt (gsi_start_phis (return_bb).phi ());
  gimple *return_stmt = gimple_build_return (result);
  gimple_stmt_iterator gsi = gsi_last_bb (return_bb);
  gsi_insert_after (&gsi, return_stmt, GSI_NEW_STMT);

  free_dominance_info (CDI_DOMINATORS);
  update_ssa (TODO_update_ssa);

  cgraph_node::create (fn_decl);
  cgraph_node::add_new_function (fn_decl, true);
  cgraph_edge::rebuild_edges ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Create %s field function:\n",
		   decompress ? "decompress" : "compress");
      dump_function_to_file (cfun->decl, dump_file, dump_flags);
    }

  pop_cfun ();

  return fn_decl;
}

/* Insert code for values in the bound:
    if (arg <= high_bound && arg >= low_bound)
      return arg;

   Return the exit_edge of the if region, whose dest is the return block.
 */

edge
ipa_struct_reorg::create_normal_part (fc_cond *fcond)
{
  edge true_e = NULL;
  edge false_e = NULL;
  tree arg = DECL_ARGUMENTS (cfun->decl);

  /* Create 'arg <= high_bound'.  */
  basic_block bb = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  tree tmp_arg = fold_convert (TREE_TYPE (fcond->high_bound), arg);
  tree cond = build2 (LE_EXPR, boolean_type_node, tmp_arg, fcond->high_bound);
  edge exit_e = create_empty_if_region_on_edge (single_succ_edge (bb), cond);
  extract_true_false_edges_from_block (single_succ (bb), &true_e, &false_e);

  /* Create 'arg >= low_bound'.  */
  bb = true_e->dest;
  tmp_arg = fold_convert (TREE_TYPE (fcond->low_bound), arg);
  cond = build2 (GE_EXPR, boolean_type_node, tmp_arg, fcond->low_bound);
  create_empty_if_region_on_edge (single_succ_edge (bb), cond);
  extract_true_false_edges_from_block (single_succ (bb), &true_e, &false_e);

  /* Return the original value on true_edge.  */
  bb = true_e->dest;
  tree return_type = TREE_TYPE (TREE_TYPE (cfun->decl));
  tree val = make_ssa_name (return_type);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  APPEND_GASSIGN_1 (gsi, val, NOP_EXPR, arg);

  basic_block return_bb = single_pred (EXIT_BLOCK_PTR_FOR_FN (cfun));
  redirect_edge_succ (single_succ_edge (bb), return_bb);
  gphi *phi = gsi_start_phis (return_bb).phi ();
  add_phi_arg (phi, val, single_succ_edge (bb), UNKNOWN_LOCATION);

  return exit_e;
}

/* Insert conversion code to compress/decompress special values.  */

void
ipa_struct_reorg::create_conversion_part (fc_cond *fcond, edge e, bool decomp)
{
  edge exit_e = e;
  basic_block return_bb = single_pred (EXIT_BLOCK_PTR_FOR_FN (cfun));
  HOST_WIDE_INT reserved_value = tree_to_uhwi (fcond->high_bound) + 1;
  for (unsigned i = 0; i < fcond->special_values.length (); i++)
    {
      basic_block bb = exit_e->src;
      tree special_cst = build_int_cst (signed_type_for (fcond->old_type),
					fcond->special_values[i]);
      tree compressed_cst = build_int_cst (fcond->new_type, reserved_value);

      if (i == fcond->special_values.length () - 1)
	{
	  /* Omit condition check for the last special value.  */
	  redirect_edge_and_branch (single_succ_edge (bb), return_bb);
	  gphi *phi = gsi_start_phis (return_bb).phi ();
	  add_phi_arg (phi, decomp ? special_cst : compressed_cst,
		       single_succ_edge (bb), UNKNOWN_LOCATION);
	}
      else
	{
	  tree arg = DECL_ARGUMENTS (cfun->decl);
	  tree cond = build2 (EQ_EXPR, boolean_type_node, arg,
			      decomp ? compressed_cst : special_cst);
	  exit_e = create_empty_if_region_on_edge (exit_e, cond);
	  edge true_e = NULL;
	  edge false_e = NULL;
	  extract_true_false_edges_from_block (single_succ (bb), &true_e,
					       &false_e);
	  redirect_edge_and_branch (single_succ_edge (true_e->dest),
				    return_bb);
	  gphi *phi = gsi_start_phis (return_bb).phi ();
	  add_phi_arg (phi, decomp ? special_cst : compressed_cst,
		       single_succ_edge (true_e->dest), UNKNOWN_LOCATION);
	  reserved_value++;
	}
    }
}

void
ipa_struct_reorg::clone_dynamic_fc_path (fc_type_info *info)
{
  SET_DUMP_FILE (NULL, TDF_NONE);
  for (auto *srfn : functions)
    {
      SET_CFUN (srfn);

      if (srfn->partial_clone_p ())
	clone_partial_func (info, srfn);
      else
	clone_whole_func (srfn);
    }
}

/* start_bb:   if (dfc.path)
		  /        \
	       false       true
		/            \
	   origin-bbs     clone-bbs
 */
void
ipa_struct_reorg::clone_partial_func (fc_type_info *info, srfunction *srfn)
{
  calculate_dominance_info (CDI_DOMINATORS);

  fc_path_info &path = srfn->fc_path;
  auto_vec<basic_block> &reach_bbs = path.reach_bbs;
  gimple *start_stmt = path.start_stmt;
  basic_block fclose_bb = reach_bbs[0];

  gcc_assert (fclose_bb == gimple_bb (start_stmt));
  edge e = split_block (fclose_bb, start_stmt);
  reach_bbs[0] = e->dest;

  unsigned n = reach_bbs.length ();
  basic_block *origin_bbs = new basic_block[n];
  for (unsigned i = 0; i < reach_bbs.length (); i++)
    origin_bbs[i] = reach_bbs[i];

  /* 1. Clone blocks reachable from start point.  */
  initialize_original_copy_tables ();
  basic_block *cloned_bbs = new basic_block[n];
  copy_bbs (origin_bbs, n, cloned_bbs, NULL, 0, NULL, fclose_bb->loop_father,
	    fclose_bb, true);
  delete[] origin_bbs;

  /* Add phis for edges from copied bbs.  */
  add_phi_args_after_copy (cloned_bbs, n, NULL);
  free_original_copy_tables ();

  path.cloned_bbs.reserve (n);
  for (unsigned i = 0; i < n; i++)
    path.cloned_bbs.safe_push (cloned_bbs[i]);
  delete[] cloned_bbs;

  /* 2. Add if-else on dfc.path.  */
  basic_block checking_bb = split_edge (e);
  gimple_stmt_iterator gsi = gsi_last_bb (checking_bb);
  tree dfc_path_ssa = make_ssa_name (info->dfc_path);
  gassign *assign = gimple_build_assign (dfc_path_ssa, info->dfc_path);
  gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
  gcond *dfc_path_cond = gimple_build_cond_from_tree (dfc_path_ssa,
						      NULL_TREE, NULL_TREE);
  gsi_insert_after (&gsi, dfc_path_cond, GSI_NEW_STMT);

  e = single_succ_edge (checking_bb);
  e->flags = (e->flags & ~EDGE_FALLTHRU) | EDGE_FALSE_VALUE;

  make_edge (checking_bb, path.cloned_bbs[0], EDGE_TRUE_VALUE);

  /* Necessary for visiting call stmts.  */
  cgraph_edge::rebuild_edges ();
  free_dominance_info (CDI_DOMINATORS);

  if (loops_state_satisfies_p (LOOPS_NEED_FIXUP))
    {
      calculate_dominance_info (CDI_DOMINATORS);
      fix_loop_structure (NULL);
    }

  update_ssa (TODO_update_ssa);
}

void
ipa_struct_reorg::clone_whole_func (srfunction *srfn)
{
  cgraph_node *new_node;
  cgraph_node *node = srfn->node;

  statistics_counter_event (NULL, "Create new function", 1);
  new_node = node->create_version_clone_with_body (vNULL, NULL, NULL, NULL,
						   NULL, "dfc");
  new_node->can_change_signature = node->can_change_signature;
  new_node->make_local ();

  srfunction *new_srfn = new srfunction (new_node);
  if (srfn->is_safe_func)
    {
      safe_functions.add (new_srfn->node);
      new_srfn->is_safe_func = true;
    }

  srfn->fc_path.cloned_func = new_srfn;
}

/* Rewrite dynamic shadow fields in cloned path.  */

void
ipa_struct_reorg::rewrite_dynamic_shadow_fields (fc_type_info *info)
{
  if (info->dynamic_shadow_fields.is_empty ())
    return;

  for (auto *access : info->type->accesses)
    {
      srfield *srf = access->field;
      if (!srf || !srf->fc_f || !srf->fc_f->original)
	continue;

      /* Skip statements in original path.  */
      srfunction *srfn = access->function;
      gimple *stmt = access->stmt;
      if (srfn->fc_path.cloned_func
	  || (srfn->partial_clone_p ()
	      && !srfn->fc_path.cloned_bbs.contains (gimple_bb (stmt))))
	continue;

      SET_CFUN (srfn);

      if (access->write_p ())
	{
	  /* Remove stmt by replacing lhs by a dummy ssa.  */
	  tree lhs = gimple_assign_lhs (stmt);
	  tree dummy_ssa = make_ssa_name (TREE_TYPE (lhs));
	  gimple_assign_set_lhs (stmt, dummy_ssa);
	  update_stmt (stmt);
	}
      else if (access->read_p ())
	modify_shadow_read (stmt, access->index, srf->fc_f, access->base);
      else
	gcc_unreachable ();
    }
}

/* Rewrite functions either partially or wholely.  */

void
ipa_struct_reorg::rewrite_dynamic_fc_path ()
{
  for (auto *srfn : functions)
    {
      if (srfn->partial_clone_p ())
	{
	  SET_CFUN (srfn);

	  /* 2.1 rewrite the original function for each path.  */
	  rewrite_partial_func (srfn);
	  clean_func_after_rewrite (srfn);
	}
      else
	{
	  /* 2.2 rewrite the cloned function for each path.  */
	  srfunction *cloned_func = srfn->fc_path.cloned_func;
	  SET_CFUN (cloned_func);

	  rewrite_whole_func (cloned_func);
	  clean_func_after_rewrite (cloned_func);
	}
    }
}

void
ipa_struct_reorg::record_dfc_path_info (fc_type_info *info)
{
  /* 1. record accesse info for cloned stmts.  */
  for (auto *srfn : functions)
    {
      SET_DUMP_FILE (NULL, TDF_NONE);
      if (srfn->partial_clone_p ())
	{
	  record_function (srfn->node, srfn);
	}
      else
	{
	  srfunction *cloned_srfn = srfn->fc_path.cloned_func;
	  record_function (cloned_srfn->node, cloned_srfn);
	  prune_function (cloned_srfn);
	}
    }

  prune_globals ();
  gcc_assert (!info->type->has_escaped ());

  /* 2. collect closure info for cloned paths.  */
  collect_closure_info_dynamic (info);
}

/* Collect the closure info for all dynamic-fc cloned paths.  */

void
ipa_struct_reorg::collect_closure_info_dynamic (fc_type_info *info)
{
  for (auto *cond : info->fc_conds)
    {
      fc_field_class *field_class = cond->field_class;
      for (auto *srfn : functions)
	{
	  if (srfn->partial_clone_p ())
	    collect_closure_info_partial (srfn, &field_class->closure);
	  else
	    collect_closure_info_whole (srfn, &field_class->closure);
	}
    }
}

/* Collect closure info for partially cloned function SRFN in dynamic fc.  */

void
ipa_struct_reorg::collect_closure_info_partial (srfunction *srfn,
						fc_closure *cinfo)
{
  closure_helper helper (cinfo);

  for (auto *bb : srfn->fc_path.reach_bbs)
    helper.record_origin_closure (bb);

  helper.reset_uid ();

  for (unsigned i = 0; i < srfn->fc_path.reach_bbs.length (); i++)
    helper.add_cloned_closure (srfn->fc_path.cloned_bbs[i]);
}

/* Collect closure info for wholely cloned function SRFN in dfc.  */

void
ipa_struct_reorg::collect_closure_info_whole (srfunction *srfn,
					      fc_closure *cinfo)
{
  closure_helper helper (cinfo);

  basic_block bb = NULL;
  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (srfn->node->decl))
    helper.record_origin_closure (bb);

  helper.reset_uid ();

  srfunction *cloned_srfn = srfn->fc_path.cloned_func;
  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (cloned_srfn->node->decl))
    helper.add_cloned_closure (bb);
}

void
ipa_struct_reorg::rewrite_partial_func (srfunction *srfn)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Before rewrite: %s\n", srfn->node->name ());
      dump_function_to_file (current_function_decl, dump_file,
			     dump_flags | TDF_VOPS);
      FC_DUMP_MSG ("Start to rewrite: %s\n", srfn->node->name ());
      fprintf (dump_file, "\n\n");
    }

  srfn->create_new_decls ();

  /* Rewrite each related stmts in the current path.  */
  for (unsigned i = 0; i < srfn->fc_path.reach_bbs.length (); i++)
    rewrite_block (srfn->fc_path.cloned_bbs[i]);
}

void
ipa_struct_reorg::rewrite_whole_func (srfunction *srfn)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Before rewrite: %s\n", srfn->node->name ());
      dump_function_to_file (current_function_decl, dump_file,
			     dump_flags | TDF_VOPS);
      FC_DUMP_MSG ("Start to rewrite: %s\n", srfn->node->name ());
    }

  create_new_args (srfn->node);
  srfn->create_new_decls ();

  basic_block bb = NULL;
  FOR_EACH_BB_FN (bb, cfun)
    rewrite_block (bb);
}

void
ipa_struct_reorg::clean_func_after_rewrite (srfunction *srfn)
{
  if (!srfn->partial_clone_p ())
    for (auto *srd : srfn->args)
      release_srdecl_ssa_name (srd);

  for (auto *srd : srfn->decls)
    release_srdecl_ssa_name (srd);

  {
    SET_DUMP_FILE (NULL, TDF_NONE);
    update_ssa (TODO_update_ssa_only_virtuals);

    unsigned i;
    tree ssa_name;
    FOR_EACH_SSA_NAME (i, ssa_name, cfun)
      {
	if (SSA_NAME_IN_FREE_LIST (ssa_name))
	  continue;

	gimple *stmt = SSA_NAME_DEF_STMT (ssa_name);

	if (!stmt || (!SSA_NAME_IS_DEFAULT_DEF (ssa_name)
		      && !gimple_bb (stmt)))
	  release_ssa_name (ssa_name);
      }

    if (flag_tree_pta)
      compute_may_aliases ();

    remove_unused_locals ();
    cgraph_edge::rebuild_edges ();
    free_dominance_info (CDI_DOMINATORS);
  }

  if (dump_file)
    {
      FC_DUMP_MSG ("After rewrite: %s\n", srfn->node->name ());
      dump_function_to_file (current_function_decl, dump_file,
			     dump_flags | TDF_VOPS);
      fprintf (dump_file, "\n\n");
    }
}

void
ipa_struct_reorg::dynamic_fc_rewrite_assign (gimple *stmt, tree rhs,
					     tree &newlhs, tree &newrhs)
{
  fc_closure *closure = cur_srfd->get_closure ();
  if (closure->write_change_p (stmt))
    {
      /* For a write stmt _0->fld = rhs, should only rewrite lhs.  */
      gcc_assert (newrhs == NULL_TREE);
      tree compress_fn = cur_srfd->fc_f->cond->compress_fn;
      if (compress_fn)
	newrhs = closure->convert_rhs (rhs, compress_fn);
    }
  else if (closure->read_change_p (stmt))
    {
      /* For a read stmt lhs = _0->fld, should only rewrite rhs.  */
      gcc_assert (newlhs == NULL_TREE);
      tree decompress_fn = cur_srfd->fc_f->cond->decompress_fn;
      if (decompress_fn)
	newrhs = closure->convert_rhs (newrhs, decompress_fn);
    }
  else if (!closure->unchange_p (stmt))
    gcc_unreachable ();
}

/* Add code for dynamic checking and data compressing.  */

void
ipa_struct_reorg::add_dynamic_checking (fc_type_info *info)
{
  basic_block bb = gimple_bb (info->fclose_stmt);
  gcc_assert (single_succ_p (bb));

  SET_CFUN (info->start_srfn);

  insert_code_calc_dfc_path (info);
  insert_code_compress_data (info, single_succ_edge (bb));

  SET_DUMP_FILE (NULL, TDF_NONE);
  cgraph_edge::rebuild_edges ();
  update_ssa (TODO_update_ssa_only_virtuals);
}

/* Insert dynamic checking code to calculate info->dfc_path.  */

void
ipa_struct_reorg::insert_code_calc_dfc_path (fc_type_info *info)
{
  insert_code_calc_max_min_val (info);
  gimple_stmt_iterator gsi = gsi_for_stmt (info->fclose_stmt);
  tree dfc_path = insert_code_calc_cond (info, &gsi);
  insert_code_check_init_const (info, &gsi, dfc_path);

  /* Store dfc_path to global var.  */
  gimple *dfc_path_stmt = gimple_build_assign (info->dfc_path, dfc_path);
  gsi_insert_after (&gsi, dfc_path_stmt, GSI_NEW_STMT);

  basic_block bb = gimple_bb (info->fclose_stmt);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\n");
      FC_DUMP_MSG ("Insert code to calculate dfc.path\n");
      dump_bb (dump_file, bb, 0, TDF_DETAILS);
    }
}

/* Insert code to calculate min and max after input_ssa inside the loop.  */

void
ipa_struct_reorg::insert_code_calc_max_min_val (fc_type_info *info)
{
  basic_block bb = gimple_bb (info->input_stmt);
  class loop *loop = bb->loop_father;
  edge latch_edge = loop_latch_edge (loop);
  for (unsigned i = 0; i < info->fc_conds.length (); i++)
    {
      fc_cond *cond = info->fc_conds[i];
      /* Use the old type for min and max value, as they will be used to
	 compare with the input ssa, which is with old type.  */
      tree ssa_type = TREE_TYPE (cond->fields[0]->input_ssa);
      char *min_name = append_suffix ("dfc.min_cond.", i);
      char *max_name = append_suffix ("dfc.max_cond.", i);
      cond->min_val = make_temp_ssa_name (ssa_type, NULL, min_name);
      cond->max_val = make_temp_ssa_name (ssa_type, NULL, max_name);

      /* Insert phi for min and max in loop header.  */
      gphi *min_phi = create_phi_node (cond->min_val, loop->header);
      gphi *max_phi = create_phi_node (cond->max_val, loop->header);

      /* For the input_ssa of each fc fields, we calculate min and max.
	 Assume all of the fc_fields have been sorted in terms of the
	 position of input_ssa.  We should always access an input_ssa in
	 forward direction.  This way, all fields' input will be used to
	 update min_val and max_val in order.  */
      tree min_val = cond->min_val;
      tree max_val = cond->max_val;
      hash_set<tree> input_ssa;
      for (auto *fc_f : cond->fields)
	{
	  /* We handle the same input_ssa only once.  */
	  if (input_ssa.contains (fc_f->input_ssa))
	    continue;

	  input_ssa.add (fc_f->input_ssa);
	  gcc_assert (TREE_TYPE (fc_f->input_ssa) == ssa_type);

	  /* Insert new stmt immediately after input_ssa.  */
	  gimple *def_stmt = SSA_NAME_DEF_STMT (fc_f->input_ssa);
	  gimple_stmt_iterator input_gsi = gsi_for_stmt (def_stmt);
	  bb = gimple_bb (def_stmt);

	  /* min = (input < min) ? input : min_phi */
	  tree min_cmp = fold_build2 (LT_EXPR, boolean_type_node,
				      fc_f->input_ssa, min_val);
	  tree input_min_rhs = build_cond_expr (min_cmp, fc_f->input_ssa,
						min_val);
	  min_val = make_temp_ssa_name (ssa_type, NULL, min_name);
	  gimple *min_stmt = gimple_build_assign (min_val, input_min_rhs);
	  gsi_insert_after (&input_gsi, min_stmt, GSI_NEW_STMT);

	  /* max = (input < max) ? input : max_phi */
	  tree max_cmp = fold_build2 (GT_EXPR, boolean_type_node,
				      fc_f->input_ssa, max_val);
	  tree input_max_rhs = build_cond_expr (max_cmp, fc_f->input_ssa,
						max_val);
	  max_val = make_temp_ssa_name (ssa_type, NULL, max_name);
	  gimple *max_stmt = gimple_build_assign (max_val, input_max_rhs);
	  gsi_insert_after (&input_gsi, max_stmt, GSI_NEW_STMT);
	}
      free (min_name);
      free (max_name);

      /* Add input_min_rhs and input_max_rhs phis.  */
      add_phi_arg (min_phi, min_val, latch_edge, UNKNOWN_LOCATION);
      add_phi_arg (max_phi, max_val, latch_edge, UNKNOWN_LOCATION);
      edge entry_edge = NULL;
      edge_iterator ei;
      FOR_EACH_EDGE (entry_edge, ei, loop->header->preds)
	{
	  if (entry_edge == latch_edge)
	    continue;
	  add_phi_arg (min_phi, build_zero_cst (ssa_type), entry_edge,
		       UNKNOWN_LOCATION);
	  add_phi_arg (max_phi, build_zero_cst (ssa_type), entry_edge,
		       UNKNOWN_LOCATION);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Insert min/max calculation\n");
      dump_bb (dump_file, loop->header, 0, TDF_DETAILS);
      dump_bb (dump_file, bb, 0, TDF_DETAILS);
    }
}

/* Insert code to calculate fc_cond after fclose.  */

tree
ipa_struct_reorg::insert_code_calc_cond (fc_type_info *info,
					 gimple_stmt_iterator *gsi)
{
  tree dfc_path = boolean_true_node;
  for (auto *cond : info->fc_conds)
    {
      /* min >= low_bound */
      tree cmp_min = fold_build2 (GE_EXPR, boolean_type_node,
				  cond->min_val, cond->low_bound);

      /* max <= high_bound */
      tree cmp_max = fold_build2 (LE_EXPR, boolean_type_node,
				  cond->max_val, cond->high_bound);

      /* ret = ((min >= low_bound) && (max <= high_bound)) */
      tree cmp_ret = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				  cmp_min, cmp_max);

      /* dfc.path.tmp = dfc.path.tmp && ret */
      tree tmp = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, dfc_path,
			      cmp_ret);
      dfc_path = force_gimple_operand_gsi (gsi, tmp, true, NULL, false,
					   GSI_CONTINUE_LINKING);
    }

  return dfc_path;
}

/* Insert code to check init_const for dynamic shadow fields.  */

void
ipa_struct_reorg::insert_code_check_init_const (fc_type_info *info,
						gimple_stmt_iterator *gsi,
						tree &dfc_path)
{
  basic_block bb = gimple_bb (info->input_stmt);
  class loop *loop = bb->loop_father;
  edge latch_edge = loop_latch_edge (loop);

  for (auto *fc_f : info->dynamic_shadow_fields)
    {
      gcc_assert (fc_f->init_const);

      /* Skip a init_const that is in special_values, because the boundary
	 check for fc_cond should have cover that.  */
      tree init_const = fc_f->init_const;
      if (fc_f->input_field->cond->special_values.contains (
	    tree_to_uhwi (init_const)))
	continue;

      tree shadow_valid = make_temp_ssa_name (boolean_type_node, NULL,
					      "dfc.shadow_valid");
      gphi *shadow_valid_phi = create_phi_node (shadow_valid, loop->header);

      auto input_gsi = gsi_for_stmt (SSA_NAME_DEF_STMT (fc_f->input_ssa));
      /* input != init_const */
      tree ne_ret = fold_build2 (NE_EXPR, boolean_type_node, fc_f->input_ssa,
			      init_const);
      tree ne_tmp = force_gimple_operand_gsi (&input_gsi, ne_ret, true, NULL,
					      false, GSI_CONTINUE_LINKING);

      /* shadow_valid = shadow_valid && (input != init_const) */
      tree and_ret = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				  shadow_valid, ne_tmp);
      tree and_tmp = force_gimple_operand_gsi (&input_gsi, and_ret, true,
					       NULL, false,
					       GSI_CONTINUE_LINKING);

      /* Insert phi for shadow_valid in loop header.  */
      add_phi_arg (shadow_valid_phi, and_tmp, latch_edge, UNKNOWN_LOCATION);
      edge entry_edge = NULL;
      edge_iterator ei;
      FOR_EACH_EDGE (entry_edge, ei, loop->header->preds)
	{
	  if (entry_edge == latch_edge)
	    continue;
	  add_phi_arg (shadow_valid_phi, boolean_true_node, entry_edge,
		       UNKNOWN_LOCATION);
	}

      /* dfc.path.tmp = dfc.path.tmp && shadow_valid */
      tree tmp = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, dfc_path,
			      shadow_valid);
      dfc_path = force_gimple_operand_gsi (gsi, tmp, true, NULL, false,
					   GSI_CONTINUE_LINKING);
    }
}

/* Split edge E and insert code to compress data.  */

void
ipa_struct_reorg::insert_code_compress_data (fc_type_info *info, edge e)
{
  if (!dom_info_available_p (CDI_DOMINATORS))
    calculate_dominance_info (CDI_DOMINATORS);
  if (!loops_state_satisfies_p (LOOPS_HAVE_PREHEADERS))
    loop_optimizer_init (LOOPS_HAVE_PREHEADERS);
  record_loop_exits ();

  auto_vec<tree> array_names;
  auto_vec<tree> size_names;
  basic_block bb = e->src;
  gimple_stmt_iterator gsi = gsi_last_bb (bb);

  /* Generate SSA_NAMEs for fc_array, if they are global addresses.  */
  for (auto *array : info->fc_arrays)
    {
      array_names.safe_push (generate_ssa_name (array->var, &gsi));
      size_names.safe_push (generate_ssa_name (array->size, &gsi));
    }

  insert_code_compress_variant (info, bb, array_names, size_names);
}

/* Insert code tot compress hot data for a fc_variant.  */

void
ipa_struct_reorg::insert_code_compress_variant (
  fc_type_info *info, basic_block bb, const auto_vec<tree> &array_names,
  const auto_vec<tree> &size_names)
{
  edge true_e = NULL;
  edge false_e = NULL;
  edge entry_e = single_succ_edge (bb);
  create_empty_if_region_on_edge (entry_e, info->dfc_path);
  extract_true_false_edges_from_block (entry_e->dest, &true_e, &false_e);

  /* Create function decl and node for compressed single object.  */
  create_compress_object_fn (info);

  edge current_e = true_e;
  insert_code_compress_array (info, current_e, array_names, size_names);
  insert_code_modify_refs (info, current_e);
}

/* For each variable(array) to compress, insert loop like:
     <bb START> :
       _1 = ARRAY_BASE;
       _2 = ARRAY_SIZE;
       goto <bb COND>;

     <bb COND> :
       # i_1 = PHI <0, i_2(THEN)>
       if (i_1 < _2)
	 goto <bb THEN>;
       else
	 goto <bb ELSE>;

     <bb THEN> :
       dfc.compress_obj (_1, i_1);
       i_2 = i_1 + 1;

     <bb ELSE> :
   */

void
ipa_struct_reorg::insert_code_compress_array (
  fc_type_info *info, edge &e, const auto_vec<tree> &data_names,
  const auto_vec<tree> &size_names)
{
  loop_p outer_loop = gimple_bb (info->fclose_stmt)->loop_father;
  for (size_t i = 0; i < size_names.length (); ++i)
    {
      tree iv_before, iv_after;
      tree size_type = TREE_TYPE (size_names[i]);
      tree iv = create_tmp_reg (size_type, "dfc.compress_idx");
      loop_p loop
	= create_empty_loop_on_edge (e, build_zero_cst (size_type),
				     build_int_cst (size_type, 1),
				     size_names[i], iv, &iv_before,
				     &iv_after, outer_loop);

      /* Build call statement to compress a single object.  */
      basic_block latch_bb = loop->latch;
      auto gsi = gsi_last_bb (latch_bb);
      tree fndecl = info->variant->compress_object_fn;
      gcall *call = gimple_build_call (fndecl, 2, data_names[i], iv_before);
      gsi_insert_after (&gsi, call, GSI_NEW_STMT);
      cgraph_node *node = cgraph_node::get (current_function_decl);
      cgraph_node *new_node = cgraph_node::get (fndecl);
      node->create_edge (new_node, call, latch_bb->count);

      e = single_exit (loop);
    }
}

/* Insert code to modify all fc_refs.  */

void
ipa_struct_reorg::insert_code_modify_refs (fc_type_info *info, edge current_e)
{
  fc_variant *variant = info->variant;
  loop_p outer_loop = gimple_bb (info->fclose_stmt)->loop_father;
  for (auto *dr : info->fc_refs)
    {
      if (!dr->size)
	{
	  /* 1) fc_ref is a single ptr.  */
	  current_e = insert_code_modify_single_ref (
			current_e, dr->var, dr->source,
			TYPE_SIZE_UNIT (info->type->type),
			TYPE_SIZE_UNIT (variant->new_type));
	  continue;
	}

      /* 2) fc_ref is an array, create a loop.  */
      tree iv_before, iv_after;
      tree ptr_type = dr->orig_type ? dr->orig_type : TREE_TYPE (dr->var);
      tree size_type = TREE_TYPE (dr->size);
      loop_p loop = create_empty_loop_on_edge (
		      current_e, build_zero_cst (size_type),
		      build_int_cst (size_type, 1), dr->size,
		      create_tmp_reg (size_type, NULL), &iv_before,
		      &iv_after, outer_loop);
      /* Fetch array element.  */
      auto gsi = gsi_last_bb (loop->latch);
      tree var1 = make_ssa_name (ptr_type);
      gsi_insert_after (&gsi, gimple_build_assign (var1, dr->var),
			GSI_NEW_STMT);
      tree var_mul = make_ssa_name (long_unsigned_type_node);
      APPEND_GASSIGN_2 (gsi, var_mul, MULT_EXPR, iv_before,
			TYPE_SIZE_UNIT (TREE_TYPE (ptr_type)));
      tree var_plus = make_ssa_name (ptr_type);
      APPEND_GASSIGN_2 (gsi, var_plus, POINTER_PLUS_EXPR, var1, var_mul);
      tree ref_expr = build2 (MEM_REF, TREE_TYPE (ptr_type), var_plus,
			      build_int_cst (ptr_type, 0));
      if (dr->field)
	ref_expr = build3 (COMPONENT_REF, TREE_TYPE (dr->field), ref_expr,
			   dr->field, NULL_TREE);
      /* Modify the ref's value.  */
      insert_code_modify_single_ref (single_succ_edge (loop->latch),
				     ref_expr, dr->source,
				     TYPE_SIZE_UNIT (info->type->type),
				     TYPE_SIZE_UNIT (variant->new_type));
      current_e = single_exit (loop);
    }
}

/* Create function to compress a single object.  Return function decl.  */

void
ipa_struct_reorg::create_compress_object_fn (fc_type_info *info)
{
  /* Function declairation.  */
  tree orig_struct_type = info->type->type;
  tree orig_struct_size = TYPE_SIZE_UNIT (orig_struct_type);
  tree orig_ptr_type = build_pointer_type (orig_struct_type);
  tree size_type = TREE_TYPE (orig_struct_size);
  tree arg_types[2] = {orig_ptr_type, size_type};
  char fn_name[32];
  sprintf (fn_name, "%s", "dfc.compress_obj");
  tree fndecl = create_new_fn_decl (fn_name, 2, arg_types, void_type_node);

  /* Function arguments.  */
  tree struct_array = DECL_ARGUMENTS (fndecl);
  tree idx = TREE_CHAIN (struct_array);

  /* Push NULL cfun.  */
  push_cfun (NULL);
  basic_block bb = init_lowered_empty_function (
		     fndecl, true, profile_count::uninitialized ());

  /* Function body.  */
  /* Use a temporary struct to avoid overlapping.  */
  tree tmp_obj = create_tmp_var (orig_struct_type, "tmp");
  /* tmp = start[i];
     =>
     idx_1 = (long unsigned int) idx;
     _2 = idx_1 * sizeof (orig_struct);
     _3 = start + _2;
     tmp = *_3;
    */
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  tree offset = make_ssa_name (long_unsigned_type_node);
  APPEND_GASSIGN_2 (gsi, offset, MULT_EXPR, idx, orig_struct_size);
  tree address = make_ssa_name (orig_ptr_type);
  APPEND_GASSIGN_2 (gsi, address, POINTER_PLUS_EXPR, struct_array, offset);
  tree rhs = build2 (MEM_REF, orig_struct_type, address,
		     build_int_cst (orig_ptr_type, 0));
  APPEND_GASSIGN_1 (gsi, tmp_obj, MEM_REF, rhs);

  /* Init: new_struct* ptr = start + idx_1 * sizeof (new_struct) */
  fc_variant *variant = info->variant;
  tree new_type = variant->new_type;
  tree new_ptr_type = build_pointer_type (new_type);
  tree new_ptr = create_tmp_var (new_ptr_type, "ptr");
  offset = make_ssa_name (long_unsigned_type_node);
  APPEND_GASSIGN_2 (gsi, offset, MULT_EXPR, idx, TYPE_SIZE_UNIT (new_type));
  APPEND_GASSIGN_2 (gsi, new_ptr, POINTER_PLUS_EXPR, struct_array, offset);
  tree ref = build2 (MEM_REF, new_type, new_ptr,
		     build_int_cst (new_ptr_type, 0));

  /* Compress and assign the fields.  */
  for (auto *field : info->type->fields)
    {
      /* Skip shadow fields.  */
      if (field->fc_f && field->fc_f->original)
	continue;

      tree old_field = field->fielddecl;
      tree old_field_type = field->fieldtype;
      tree new_field = field->newfield[0] ? field->newfield[0] : old_field;
      tree new_field_type = TREE_TYPE (new_field);

      tree var = make_ssa_name (old_field_type);
      tree rhs = build3 (COMPONENT_REF, old_field_type, tmp_obj, old_field,
			 NULL_TREE);
      APPEND_GASSIGN_1 (gsi, var, COMPONENT_REF, rhs);
      if (new_field_type != old_field_type)
	{
	  fc_cond *cond = field->fc_f->cond;
	  if (cond && cond->compress_fn)
	    {
	      /* Need compressing.  */
	      /* As we may have bitfield, so cond->new_type and new_type
		 can be different.  */
	      tree compressed_var = make_ssa_name (cond->new_type);
	      gcall *stmt = gimple_build_call (cond->compress_fn, 1, var);
	      gimple_call_set_lhs (stmt, compressed_var);
	      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
	      var = compressed_var;
	    }
	  tree converted_var = make_ssa_name (new_field_type);
	  APPEND_GASSIGN_1 (gsi, converted_var, NOP_EXPR, var);
	  var = converted_var;
	}
      tree lhs = build3 (COMPONENT_REF, new_field_type, ref, new_field,
			 NULL_TREE);
      APPEND_GASSIGN_1 (gsi, lhs, MEM_REF, var);
    }

  /* Clobber and return.  */
  tree clobber = build_clobber (orig_struct_type);
  gimple *stmt = gimple_build_assign (tmp_obj, clobber);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
  stmt = gimple_build_return (NULL);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  {
    SET_DUMP_FILE (NULL, TDF_NONE);
    update_ssa (TODO_update_ssa);
  }

  cgraph_node::create (fndecl);
  cgraph_node::add_new_function (fndecl, true);
  cgraph_edge::rebuild_edges ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      FC_DUMP_MSG ("Create compress object function:\n");
      dump_function_to_file (cfun->decl, dump_file, dump_flags);
    }
  pop_cfun ();

  info->variant->compress_object_fn = fndecl;
}

/* Split edge E and insert codes to modify a single fc_ref expression.
   Return the exit edge of created codes.  */

edge
ipa_struct_reorg::insert_code_modify_single_ref (edge e, tree ref,
						 fc_array *array,
						 tree orig_size,
						 tree new_size)
{
  /* For each fc_ref, create code like:
     if (REF)
	REF = (long) ARRAY + ((long) REF - (long) ARRAY)
			     / sizeof(old_type) * sizeof(new_type);
   */

  /* 1) Create ssa_name for fc_ref.  */
  tree ref_ssa_name = create_tmp_reg (TREE_TYPE (ref));
  gimple *stmt = gimple_build_assign (ref_ssa_name, unshare_expr (ref));
  basic_block bb = split_edge (e);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  /* 2) Create the if-else structure.  */
  tree cmp = build2 (EQ_EXPR, boolean_type_node, ref_ssa_name,
		     null_pointer_node);
  edge exit_e = create_empty_if_region_on_edge (single_succ_edge (bb), cmp);
  edge true_e = NULL;
  edge false_e = NULL;
  extract_true_false_edges_from_block (single_succ (bb), &true_e, &false_e);
  gsi = gsi_last_bb (false_e->dest);

  /* 3) Create conversion codes.  */
  tree ssa_def = array->ssa_def;
  tree sub_var = make_ssa_name (ptrdiff_type_node);
  APPEND_GASSIGN_2 (gsi, sub_var, POINTER_DIFF_EXPR, ref_ssa_name, ssa_def);
  tree div_var = make_ssa_name (ptrdiff_type_node);
  APPEND_GASSIGN_2 (gsi, div_var, TRUNC_DIV_EXPR, sub_var,
		    fold_convert (ptrdiff_type_node, orig_size));
  tree mul_var = make_ssa_name (ptrdiff_type_node);
  APPEND_GASSIGN_2 (gsi, mul_var, MULT_EXPR, div_var,
		    fold_convert (ptrdiff_type_node, new_size));
  tree mul_var2 = make_ssa_name (size_type_node);
  APPEND_GASSIGN_1 (gsi, mul_var2, NOP_EXPR, mul_var);
  tree add_var = make_ssa_name (TREE_TYPE (ssa_def));
  APPEND_GASSIGN_2 (gsi, add_var, POINTER_PLUS_EXPR, ssa_def, mul_var2);

  /* 4) Store.  */
  gsi_insert_after (&gsi, gimple_build_assign (unshare_expr (ref), add_var),
		    GSI_NEW_STMT);
  return exit_e;
}

/* Init pointer size from parameter param_pointer_compression_size.  */

static void
init_pointer_size_for_pointer_compression (void)
{
  switch (param_pointer_compression_size)
    {
      case 8:
      // FALLTHRU
      case 16:
      // FALLTHRU
      case 32: compressed_size = param_pointer_compression_size; break;
      default:
	error ("Invalid pointer compression size, using the following param: "
	       "\"--param compressed-pointer-size=[8,16,32]\"");
    }
}

unsigned int
ipa_struct_reorg::execute (unsigned int opt)
{
  unsigned int ret = 0;

  if (dump_file)
    fprintf (dump_file, "\n\n====== ipa_struct_reorg level %d ======\n\n",
	     opt);

  if (opt != COMPLETE_STRUCT_RELAYOUT)
    {
      current_layout_opt_level = opt;
      /* If there is a top-level inline-asm,
	 the pass immediately returns.  */
      if (symtab->first_asm_symbol ())
	return 0;
      record_accesses ();
      prune_escaped_types ();
      if (current_layout_opt_level == STRUCT_SPLIT)
	analyze_types ();

      if (opt >= POINTER_COMPRESSION_SAFE)
	check_and_prune_struct_for_pointer_compression ();
      if (opt >= SEMI_RELAYOUT)
	check_and_prune_struct_for_semi_relayout ();
      /* Avoid doing static field compression in STRUCT_SPLIT.  */
      if (opt >= STRUCT_REORDER_FIELDS
	  && current_fc_level == fc_level::STATIC)
	check_and_prune_struct_for_field_compression ();
      ret = rewrite_functions ();
    }
  else
    {
      if (dump_file)
	fprintf (dump_file, "\n\nTry Complete Struct Relayout:\n");
      current_layout_opt_level = COMPLETE_STRUCT_RELAYOUT;
      if (symtab->first_asm_symbol ())
	return 0;
      record_accesses ();
      prune_escaped_types ();

      ret = execute_struct_relayout ();
    }

  return ret;
}

const pass_data pass_data_ipa_struct_reorg =
{
  SIMPLE_IPA_PASS, // type
  "struct_reorg",  // name
  OPTGROUP_NONE,   // optinfo_flags
  TV_IPA_STRUCT_REORG, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  0, // todo_flags_finish
};

class pass_ipa_struct_reorg : public simple_ipa_opt_pass
{
public:
  pass_ipa_struct_reorg (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_struct_reorg, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *)
  {
    unsigned int ret = 0;
    unsigned int ret_reorg = 0;
    unsigned int level = 0;
    switch (struct_layout_optimize_level)
      {
	case 6: level |= SEMI_RELAYOUT;
	// FALLTHRU
	case 5: level |= POINTER_COMPRESSION_UNSAFE;
	// FALLTHRU
	case 4: level |= POINTER_COMPRESSION_SAFE;
	// FALLTHRU
	case 3: level |= DEAD_FIELD_ELIMINATION;
	// FALLTHRU
	case 2: level |= STRUCT_REORDER_FIELDS;
	// FALLTHRU
	case 1:
	  level |= COMPLETE_STRUCT_RELAYOUT;
	  level |= STRUCT_SPLIT;
	  break;
	case 0: break;
	default: gcc_unreachable ();
      }

    if (level & POINTER_COMPRESSION_SAFE)
      init_pointer_size_for_pointer_compression ();

    if (level & SEMI_RELAYOUT)
      {
	semi_relayout_align = semi_relayout_level;
	relayout_part_size = 1 << semi_relayout_level;
      }

    current_fc_level = fc_level::NONE;
    if (flag_ipa_struct_sfc)
      current_fc_level = fc_level::STATIC;

    /* Preserved for backward compatibility, reorder fields needs run before
       struct split and complete struct relayout.  */
    if (flag_ipa_reorder_fields && level < STRUCT_REORDER_FIELDS)
      ret = ipa_struct_reorg ().execute (STRUCT_REORDER_FIELDS);

    if (level >= STRUCT_REORDER_FIELDS)
      ret = ipa_struct_reorg ().execute (level);

    /* Reset current_fc_level before struct_split and csr.  */
    current_fc_level = fc_level::NONE;

    if (ret & TODO_remove_functions)
      symtab->remove_unreachable_nodes (dump_file);

    if (level >= COMPLETE_STRUCT_RELAYOUT)
      {
	/* Preserved for backward compatibility.
	   Rewrite between STRUCT_REORDER_FIELDS and STRUCT_SPLIT has unfixed
	   problem, so avoid using them together.  */
	if (!ret)
	  ret_reorg = ipa_struct_reorg ().execute (STRUCT_SPLIT);
	if (!ret_reorg)
	  ret_reorg = ipa_struct_reorg ().execute (COMPLETE_STRUCT_RELAYOUT);
      }

    if (ret && flag_ipa_struct_dfc)
      {
	current_fc_level = fc_level::DYNAMIC;
	ret = ipa_struct_reorg ().execute_dynamic_field_compression ();
      }

    return ret | ret_reorg;
  }

}; // class pass_ipa_struct_reorg

bool
pass_ipa_struct_reorg::gate (function *)
{
  return (optimize >= 3
	  && flag_ipa_struct_reorg
	  /* Don't bother doing anything if the program has errors.  */
	  && !seen_error ()
	  && flag_lto_partition == LTO_PARTITION_ONE
	  /* Only enable struct optimizations in C since other
	     languages' grammar forbid.  */
	  && lang_c_p ()
	  /* Only enable struct optimizations in lto or whole_program.  */
	  && (in_lto_p || flag_whole_program));
}

} // anon namespace


simple_ipa_opt_pass *
make_pass_ipa_struct_reorg (gcc::context *ctxt)
{
  return new pass_ipa_struct_reorg (ctxt);
}
