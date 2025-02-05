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

struct srfield;
struct srtype;
struct sraccess;
struct srdecl;
struct srfunction;
class fc_type_info;
class fc_field;
class fc_field_class;

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

  // Constructors
  srfunction (cgraph_node *n);

  // Methods
  void add_arg (srdecl *arg);
  void dump (FILE *file);
  void simple_dump (FILE *file);

  bool check_args (void);
  void create_new_decls (void);
  srdecl *find_decl (tree);
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

  bool create_new_type (void);
  void analyze (void);
  bool has_dead_field (void);
  void mark_escape (escape_type, gimple *stmt);
  void create_global_ptr_for_pc ();
  unsigned calculate_bucket_size ();
  bool has_recursive_field_type ();
  void check_fc_fields ();
  bool has_escaped (void)
  {
    return escapes != does_not_escape;
  }
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

  fc_field *static_fc_field;
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
  bool write_type_p (tree) const;
  bool write_field_p (tree) const;
  bool read_field_p (tree) const;
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

/* All fields belong to this class should have the same type.  */

class fc_field_class
{
public:
  /* The same type for all of the fields in the class.  */
  tree fieldtype;

  /* The fields with the same type are in the same element of this vector.  */
  auto_vec<srfield *> srfields;

  fc_field_class (tree fieldtype)
    : fieldtype (fieldtype)
  {}

  void dump (FILE *) const;
  unsigned size () const;
  int get_field_index (srfield *) const;
};

/* The field for field compression.  */

class fc_field
{
public:
  tree field;
  tree new_type;

  /* This field's max value we can know at compile time.  If it is 0, it means
     the max value cannot be determined at compile time.  */
  HOST_WIDE_INT max_value;

  /* The bit width of the field if it is not zero.  */
  unsigned bits;

  /* The original field of a shadow field if it is not NULL.  */
  srfield *original;

  /* All assignments that need to be optimized as shadow.  */
  auto_vec<gimple *> shadow_stmts;

  /* The 1:1 map of shadow_stmts to indicate the current function of a shadow
     stmt belongs to.  */
  auto_vec<srfunction *> shadow_stmts_func;

  /* For static field compression.  */
  fc_field (tree field, HOST_WIDE_INT max_value, srfield *original)
    : field (field), new_type (NULL_TREE), max_value (max_value),
      bits (0), original (original)
  {}

  unsigned get_bits (void) const
  {
    return bits;
  }
};

/* The class to hold field compression type information.
   A single info object is only for one structure type.  */

class fc_type_info
{
public:
  srtype *type;

  /* The flag to control whether the type can do static field compression.  */
  bool static_fc_p = false;

  /* Multiple fields of the data struct for static compression.  */
  auto_delete_vec<fc_field> static_fc_fields;

  /* The field classes classified by field type.  */
  auto_delete_vec<fc_field_class> field_classes;

  fc_type_info (srtype *type)
    : type (type)
  {}
  fc_type_info ()
    : type (NULL)
  {}

  fc_field_class *find_field_class_by_type (tree) const;
  fc_field_class *record_field_class (srfield *);
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
