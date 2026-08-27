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

#ifndef IPA_STRUCT_REORG_H
#define IPA_STRUCT_REORG_H

namespace struct_reorg {

const int max_split = 2;

unsigned semi_relayout_align = semi_relayout_level;
unsigned relayout_part_size = 1 << semi_relayout_level;

template <typename type>
struct auto_vec_del : auto_vec<type *>
{
  ~auto_vec_del ();
};

template <typename T>
auto_vec_del<T>::~auto_vec_del (void)
{
  unsigned i;
  T *t;
  FOR_EACH_VEC_ELT (*this, i, t)
    {
      delete t;
    }
}

enum escape_type
{
  does_not_escape,
#define DEF_ESCAPE(ENUM, TEXT) ENUM,
#include "escapes.def"
  escape_max_escape
};

const char *escape_type_string[escape_max_escape - 1] =
{
#define DEF_ESCAPE(ENUM, TEXT) TEXT,
#include "escapes.def"
};

enum class check_ref_result
{
  NEW,
  DUPLICATIVE,
  ERROR,
};

struct srfield;
struct srtype;
struct sraccess;
struct srdecl;
struct srfunction;
class fc_closure;
class fc_type_info;
class fc_field;
class fc_field_class;
class fc_cond;

class fc_path_info
{
public:
  enum direction
  {
    PRED,
    SUCC
  };

public:
  /* The start stmt to clone blocks.  If it is NULL, the whole function is
     cloned (i.e. versioning).  */
  gimple *start_stmt = NULL;

  /* Blocks reachable from the start_stmt.  */
  auto_vec<basic_block> reach_bbs;

  /* Blocks that can reach the start_stmt.  */
  auto_vec<basic_block> pre_bbs;

  /* Cloned basic blocks of reach_bbs.  */
  auto_vec<basic_block> cloned_bbs;

  /* Cloned whole function versions.  */
  srfunction *cloned_func = NULL;

  fc_path_info ()
  {}
  ~fc_path_info ();

  bool collect_blocks (gimple *, direction);
};

struct srfunction
{
  cgraph_node *node;
  auto_vec<srdecl *> args;
  auto_vec<srdecl *> globals;
  auto_vec_del<srdecl> decls;
  srdecl *record_decl (srtype *, tree, int arg, tree orig_type = NULL);

  srfunction *old;
  cgraph_node *newnode;
  srfunction *newf;

  bool is_safe_func;

  fc_path_info fc_path;

  // Constructors
  srfunction (cgraph_node *n);

  // Methods
  void add_arg (srdecl *arg);
  void dump (FILE *file);
  void simple_dump (FILE *file);

  bool check_args (void);
  void create_new_decls (void);
  srdecl *find_decl (tree);

  bool partial_clone_p ();
  bool entry_function_p ();
};

struct srglobal : private srfunction
{
  srglobal ()
    : srfunction (NULL)
  {}

  using srfunction::dump;
  using srfunction::create_new_decls;
  using srfunction::find_decl;
  using srfunction::record_decl;
  using srfunction::decls;
};

struct srtype
{
  tree type;
  auto_vec_del<srfield> fields;

  // array of fields that use this type.
  auto_vec<srfield *> field_sites;

  // array of functions which use directly the type
  auto_vec<srfunction *> functions;

  auto_vec_del<sraccess> accesses;
  bool chain_type;

private:
  escape_type escapes;

public:
  tree newtype[max_split];
  tree pc_gptr;
  bool visited;
  bool pc_candidate;
  bool has_legal_alloc_num;
  /* Negative number means it has illegal allocated arrays
     that we do not optimize.  */
  int has_alloc_array;
  bool semi_relayout;
  hash_map<tree, unsigned long> new_field_offsets;
  unsigned bucket_parts;
  unsigned bucket_size;

  fc_type_info *fc_info;

  // Constructors
  srtype (tree type);

  // Methods
  void dump (FILE *file);
  void simple_dump (FILE *file);
  void add_function (srfunction *);
  void add_access (sraccess *a)
  {
    accesses.safe_push (a);
  }
  void add_field_site (srfield *);

  srfield *find_field (unsigned HOST_WIDE_INT offset);
  srfield *find_field_by_decl (tree);

  bool create_new_type (void);
  void analyze (void);
  bool has_dead_field (void);
  void mark_escape (escape_type, gimple *stmt);
  void create_global_ptr_for_pc ();
  unsigned calculate_bucket_size ();
  bool has_recursive_field_type ();
  void check_fc_fields ();
  bool reorg_name_p ();
  bool has_escaped (void);
  const char *escape_reason (void)
  {
    if (!has_escaped ())
      return NULL;
    return escape_type_string[escapes - 1];
  }
  bool escaped_rescusive (void)
  {
    return escapes == escape_rescusive_type;
  }
  bool has_new_type (void)
  {
    return newtype[0] && newtype[0] != type;
  }
};

/* Bitflags used for determining if a field
     is never accessed, read or written.  */
const unsigned EMPTY_FIELD = 0x0u;
const unsigned READ_FIELD = 0x01u;
const unsigned WRITE_FIELD = 0x02u;

struct srfield
{
  unsigned HOST_WIDE_INT offset;
  tree fieldtype;
  tree fielddecl;
  srtype *base;
  srtype *type;

  unsigned clusternum;

  tree newfield[max_split];
  unsigned field_access; /* FIELD_DECL -> bitflag (use for dfe).  */

  fc_field *fc_f;
  fc_field_class *field_class;

  // Constructors
  srfield (tree field, srtype *base);

  // Methods
  void dump (FILE *file);
  void simple_dump (FILE *file);

  void create_new_fields (tree newtype[max_split],
			  tree newfields[max_split],
			  tree newlast[max_split]);
  void reorder_fields (tree newfields[max_split], tree newlast[max_split],
		       tree &field);
  void create_new_reorder_fields (tree newtype[max_split],
				  tree newfields[max_split],
				  tree newlast[max_split]);
  bool dead_field_p ();
  bool dfc_type_change_p ();
  fc_closure *get_closure ();
};

struct sraccess
{
  unsigned index;
  tree expr;
  gimple *stmt;
  cgraph_node *node;

  srfunction *function;
  srtype *type;
  tree base;
  // NULL field means the whole type is accessed
  srfield *field;

  // Constructors
  sraccess (tree, gimple *, cgraph_node *, srfunction *,
	    srtype *, tree, srfield *);

  // Methods
  void dump (FILE *file) const;
  bool write_field_p (tree = NULL_TREE) const;
  bool read_field_p (tree = NULL_TREE) const;
  bool write_p () const;
  bool read_p () const;
};

struct srdecl
{
  srtype *type;
  tree decl;
  tree func;
  /* -1 : not an argument
     -2 : static chain
  */
  int argumentnum;

  bool visited;

  tree newdecl[max_split];

  /* Auxiliary record complete original type information of the void* type.  */
  tree orig_type;

  // Constructors
  srdecl (srtype *type, tree decl, int argumentnum = -1, tree orgtype = NULL);

  // Methods
  void dump (FILE *file);
  bool has_new_decl (void)
  {
    return newdecl[0] && newdecl[0] != decl;
  }
};

/* Describe stmt closure to help rewrite.  The closure could be either array
   pointers for the same memory space, or normal data without calculation.  */

class fc_closure
{
public:
  /* The stmts for read/write of the fc field.  For read/write_change, we need
     to add convert function for read and write respectively.  */
  hash_set<gimple *> read_unchange_set;
  hash_set<gimple *> read_change_set;
  hash_set<gimple *> write_unchange_set;
  hash_set<gimple *> write_change_set;

  /* Record the known special rhs assigned to this fc field.  */
  hash_map<tree, HOST_WIDE_INT> write_special_rhs;

  void add_read_change (gimple *);
  bool read_change_p (gimple *);
  void add_read_unchange (gimple *);
  bool read_unchange_p (gimple *);
  void add_write_change (gimple *);
  bool write_change_p (gimple *);
  void add_write_unchange (gimple *);
  bool write_unchange_p (gimple *);
  bool change_p (gimple *);
  bool unchange_p (gimple *);

  /* Call compress/decompress function FN for RHS.  */
  tree convert_rhs (tree, tree);
};

class closure_helper
{
private:
  /* The unique id for assign stmts used in collecting closure info.  */
  int uid;
  fc_closure *cinfo;

  auto_bitmap read_change_map;
  auto_bitmap write_change_map;
  auto_bitmap read_unchange_map;
  auto_bitmap write_unchange_map;

public:
  closure_helper (fc_closure *cinfo)
    : uid (0), cinfo (cinfo)
  {}

  void record_origin_closure (basic_block);
  void add_cloned_closure (basic_block);
  void reset_uid ();
};

/* All fields belong to this class should have the same type.  */

class fc_field_class
{
public:
  /* The same type for all of the fields in the class.  */
  tree fieldtype = NULL_TREE;

  /* The fields with the same type are in the same element of this vector.  */
  auto_vec<srfield *> srfields;

  /* Back reference to corresponding fc_cond.  */
  fc_cond *cond = NULL;

  /* Record all info related if the class is an identified closure.  */
  fc_closure closure;

  fc_field_class (tree fieldtype)
    : fieldtype (fieldtype)
  {}

  void dump (FILE *) const;
  unsigned size () const;
  int get_field_index (srfield *) const;
};

/* The fc condition for a specified data type.  Multiple vars with the same
   data type can map to the same fc_cond object.  */

class fc_cond
{
public:
  /* The old field data type for this condition.  */
  tree old_type = NULL_TREE;

  /* The new field data type for this condition.  */
  tree new_type = NULL_TREE;

  /* The bit width of the new_type if it is a bit field.  */
  unsigned bits = 0;

  /* The type class to which all of fc_fields in this condition belongs.  */
  fc_field_class *field_class = NULL;

  /* May have multiple fields mapping to this condition, as they have the
     same data type.  */
  auto_vec<fc_field *> fields;

  /* The condition variable we want to check.  */
  tree cond_var = NULL_TREE;

  /* The vars to hold the min and max input.  */
  tree min_val = NULL_TREE;
  tree max_val = NULL_TREE;

  /* The constant value we need to check at run-time.  */
  tree low_bound = NULL_TREE;
  tree high_bound = NULL_TREE;

  /* Hold all special constant values for this condition type.  */
  auto_vec<HOST_WIDE_INT> special_values;

  /* Compress and decompress function decls, if there're special values.  */
  tree compress_fn = NULL_TREE;
  tree decompress_fn = NULL_TREE;

  fc_cond (tree old_type = NULL_TREE)
    : old_type (old_type)
  {}
  ~fc_cond ()
  {}
};

/* The field for field compression.  */

class fc_field
{
public:
  tree field = NULL_TREE;
  tree new_type = NULL_TREE;

  /* This field's max value we can know at compile time.  If it is 0, it means
     the max value cannot be determined at compile time.  */
  HOST_WIDE_INT max_value = 0;

  /* The bit width of the field if it is not zero.  */
  unsigned bits = 0;

  /* The total number of static reference count.  The bigger, the smaller
     size for dynamic field compression.  */
  unsigned ref_cnt = 0;

  /* The original field of a shadow field if it is not NULL.  */
  srfield *original = NULL;

  /* A dynamic shadow field must have a input fc_field counter part.  */
  fc_field *input_field = NULL;

  /* Init constants of the original srfield.  */
  tree init_const = NULL_TREE;

  /* All assignments that need to be optimized as shadow.  */
  auto_vec<gimple *> shadow_stmts;

  /* The 1:1 map of shadow_stmts to indicate the current function of a shadow
     stmt belongs to.  */
  auto_vec<srfunction *> shadow_stmts_func;

  /* The input var that is read from a file, and assigned to this fc_field.  */
  tree input_var = NULL_TREE;

  /* The ssa for the input_var.  */
  tree input_ssa = NULL_TREE;

  /* The condition var descriptor for this field.  */
  fc_cond *cond = NULL;

  /* For static field compression.  */
  fc_field (tree field, HOST_WIDE_INT max_value, srfield *original)
    : field (field), max_value (max_value), original (original)
  {}

  /* For dynamic field compression.  */
  fc_field (tree field, tree input_var, tree input_ssa)
    : field (field), input_var (input_var), input_ssa (input_ssa)
  {}

  unsigned get_bits (void) const
  {
    return bits;
  }
};

/* A hot array that needs to be cached.  */

class fc_array
{
public:
  /* The variable declaration that holds the data to be cached.  */
  tree var = NULL_TREE;

  /* The size expr to help data initialization.  */
  tree size = NULL_TREE;

  /* If fc_array is allocated in start-function, record the ssa_name of
     allocated ptr, we may need this to create fc_refs.  */
  tree ssa_def = NULL_TREE;

  /* varpool_node for a global fc_array variable.  We may need this to search
     for fc_refs.  */
  varpool_node *vnode = NULL;

  fc_array (tree var, tree size, tree ssa_def, varpool_node *vnode)
    : var (var), size (size), ssa_def (ssa_def), vnode (vnode)
  {}
  ~fc_array ()
  {}
};

/* A variable that needs to be modified according to the caching data.  */

class fc_ref
{
public:
  /* The variable's declaration.  */
  tree var = NULL_TREE;

  /* "real" type, for void*.  */
  tree orig_type = NULL_TREE;

  /* fc_array referred by this variable.  */
  fc_array *source = NULL;

  /* Number of elements, if this variable is an array.  */
  tree size = NULL_TREE;

  /* For array of records, this is the field to be modified.  */
  tree field = NULL_TREE;

  fc_ref (tree var, tree orig_type, fc_array *source,
	   tree size, tree field)
    : var (var), orig_type (orig_type), source (source),
      size (size), field (field)
  {}
  ~fc_ref ()
  {}

  void dump (FILE *) const;
};

/* Variants for different dynamic checking condition combinations.  */
class fc_variant
{
public:
  /* New structure type.  */
  tree new_type = NULL_TREE;

  /* The function to compress a single object.  */
  tree compress_object_fn = NULL_TREE;
};

/* The class to hold field compression type information.
   A single info object is only for one structure type.  */

class fc_type_info
{
public:
  srtype *type = NULL;

  /* The flag to control whether the type can do static field compression.  */
  bool static_fc_p = false;
  bool dynamic_fc_p = false;

  /* Multiple fields of the data struct for static compression.  */
  auto_delete_vec<fc_field> static_fc_fields;

  /* Multiple fields of the data struct for dynamic compression.  */
  auto_delete_vec<fc_field> dynamic_fc_fields;

  /* Multiple fields of the data struct for dynamic shadow.  */
  auto_delete_vec<fc_field> dynamic_shadow_fields;

  /* The stmt that read data from file.  */
  gimple *input_stmt = NULL;

  /* The variable into which the data read from input stmt is assigned.  */
  tree input_var = NULL_TREE;

  /* The file handler of data file.  */
  tree input_file_handler = NULL_TREE;

  /* The fclose stmt of the data file.  */
  gimple *fclose_stmt = NULL;

  /* The function with start point.  */
  srfunction *start_srfn = NULL;

  /* All fc_array variables need to be compressed.  */
  auto_delete_vec<fc_array> fc_arrays;

  /* All variables to be modified according to compressed data.  */
  auto_delete_vec<fc_ref> fc_refs;

  /* All indivisual fc conditions.  */
  auto_delete_vec<fc_cond> fc_conds;

  /* The variant of data type after dfc.  Now we only support one variant.  */
  fc_variant *variant = NULL;

  /* The flag to indicate which path to run.  */
  tree dfc_path = NULL_TREE;

  /* The field classes classified by field type.  */
  auto_delete_vec<fc_field_class> field_classes;

  fc_type_info (srtype *type)
    : type (type)
  {}
  ~fc_type_info ();

  fc_field_class *find_field_class_by_type (tree) const;
  fc_field_class *record_field_class (srfield *);
  fc_cond *find_cond (tree) const;
  fc_cond *create_cond (tree);
  void record_cond (fc_field *);
};

/* The structure to hold necessary information for field shadow.  */

struct fc_shadow_info
{
  auto_delete_vec<auto_vec<gimple *>> pair_stmts_groups;
  auto_vec<srfunction *> pair_stmts_func;
  gimple *unpair_stmt = NULL;
  srfunction *unpair_stmt_func = NULL;
  unsigned unpair_stmt_index = 0;
};

} // namespace struct_reorg


namespace struct_relayout {

const int min_relayout_split = 8;
const int max_relayout_split = 16;

struct csrtype
{
  tree type;
  unsigned HOST_WIDE_INT old_size;
  unsigned HOST_WIDE_INT new_size;
  unsigned field_count;
  tree struct_size;

  // Constructors
  csrtype ()
    : type (NULL),
      old_size (0),
      new_size (0),
      field_count (0),
      struct_size (NULL)
  {}

  // Methods
  unsigned calculate_field_num (tree field_offset);
  void init_type_info (void);
};

} // namespace struct_relayout

#endif
