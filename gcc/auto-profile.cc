/* Read and annotate call graph profile from the auto profile data file.
   Copyright (C) 2014-2022 Free Software Foundation, Inc.
   Contributed by Dehao Chen (dehao@google.com)

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
#define INCLUDE_MAP
#define INCLUDE_SET
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gcov-io.h"
#include "diagnostic-core.h"
#include "profile.h"
#include "langhooks.h"
#include "cfgloop.h"
#include "tree-cfg.h"
#include "tree-cfgcleanup.h"
#include "tree-into-ssa.h"
#include "gimple-iterator.h"
#include "value-prof.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "ipa-inline.h"
#include "tree-inline.h"
#include "auto-profile.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include <map>
#include <vector>
#include <algorithm>

/* The following routines implements AutoFDO optimization.

   This optimization uses sampling profiles to annotate basic block counts
   and uses heuristics to estimate branch probabilities.

   There are three phases in AutoFDO:

   Phase 1: Read profile from the profile data file.
     The following info is read from the profile datafile:
        * string_table: a map between function name and its index.
        * autofdo_source_profile: a map from function_instance name to
          function_instance. This is represented as a forest of
          function_instances.
        * WorkingSet: a histogram of how many instructions are covered for a
          given percentage of total cycles. This is describing the binary
          level information (not source level). This info is used to help
          decide if we want aggressive optimizations that could increase
          code footprint (e.g. loop unroll etc.)
     A function instance is an instance of function that could either be a
     standalone symbol, or a clone of a function that is inlined into another
     function.

   Phase 2: Early inline + value profile transformation.
     Early inline uses autofdo_source_profile to find if a callsite is:
        * inlined in the profiled binary.
        * callee body is hot in the profiling run.
     If both condition satisfies, early inline will inline the callsite
     regardless of the code growth.
     Phase 2 is an iterative process. During each iteration, we also check
     if an indirect callsite is promoted and inlined in the profiling run.
     If yes, vpt will happen to force promote it and in the next iteration,
     einline will inline the promoted callsite in the next iteration.

   Phase 3: Annotate control flow graph.
     AutoFDO uses a separate pass to:
        * Annotate basic block count
        * Estimate branch probability

   After the above 3 phases, all profile is readily annotated on the GCC IR.
   AutoFDO tries to reuse all FDO infrastructure as much as possible to make
   use of the profile. E.g. it uses existing mechanism to calculate the basic
   block/edge frequency, as well as the cgraph node/edge count.
*/

#define DEFAULT_AUTO_PROFILE_FILE "fbdata.afdo"
#define DEFAULT_CACHE_MISSES_PROFILE_FILE "cmsdata.gcov"
#define DEFAULT_ADDITIONAL_PROFILE_FILE "addldata.gcov"
#define AUTO_PROFILE_VERSION 2

namespace autofdo
{

/* Intermediate edge info used when propagating AutoFDO profile information.
   We can't edge->count() directly since it's computed from edge's probability
   while probability is yet not decided during propagation.  */
#define AFDO_EINFO(e)                     ((class edge_info *) e->aux)
class edge_info
{
public:
  edge_info () : count_ (profile_count::zero ().afdo ()), annotated_ (false) {}
  bool is_annotated () const { return annotated_; }
  void set_annotated () { annotated_ = true; }
  profile_count get_count () const { return count_; }
  void set_count (profile_count count) { count_ = count; }
private:
  profile_count count_;
  bool annotated_;
};

/* pair <func_decl, count>  */
static bool
event_count_cmp (std::pair<unsigned, gcov_type> &a,
		 std::pair<unsigned, gcov_type> &b)
{
  return a.second > b.second;
}

/* Represent a source location: (function_decl, lineno).  */
typedef std::pair<tree, unsigned> decl_lineno;

/* Represent an inline stack. vector[0] is the leaf node.  */
typedef auto_vec<decl_lineno> inline_stack;

/* String array that stores function names.  */
typedef auto_vec<char *> string_vector;

/* Map from function name's index in string_table to target's
   execution count.  */
typedef std::map<unsigned, gcov_type> icall_target_map;

/* Set of gimple stmts. Used to track if the stmt has already been promoted
   to direct call.  */
typedef std::set<gimple *> stmt_set;

/* Represent count info of an inline stack.  */
class count_info
{
public:
  /* Sampled count of the inline stack.  */
  gcov_type count;

  /* Map from indirect call target to its sample count.  */
  icall_target_map targets;

  /* Whether this inline stack is already used in annotation.

     Each inline stack should only be used to annotate IR once.
     This will be enforced when instruction-level discriminator
     is supported.  */
  bool annotated;
};

/* operator< for "const char *".  */
struct string_compare
{
  bool operator()(const char *a, const char *b) const
  {
    return strcmp (a, b) < 0;
  }
};

/* Store a string array, indexed by string position in the array.  */
class string_table
{
public:
  string_table ()
  {}

  ~string_table ();

  /* For a given string, returns its index.  */
  int get_index (const char *name) const;

  /* For a given decl, returns the index of the decl name.  */
  int get_index_by_decl (tree decl) const;

  /* For a given index, returns the string.  */
  const char *get_name (int index) const;

  /* Read profile, return TRUE on success.  */
  bool read ();

private:
  typedef std::map<const char *, unsigned, string_compare> string_index_map;
  string_vector vector_;
  string_index_map map_;
};

/* Profile of a function instance:
     1. total_count of the function.
     2. head_count (entry basic block count) of the function (only valid when
        function is a top-level function_instance, i.e. it is the original copy
        instead of the inlined copy).
     3. map from source location (decl_lineno) to profile (count_info).
     4. map from callsite to callee function_instance.  */
class function_instance
{
public:
  typedef auto_vec<function_instance *> function_instance_stack;

  /* Read the profile and return a function_instance with head count as
     HEAD_COUNT. Recursively read callsites to create nested function_instances
     too. STACK is used to track the recursive creation process.  */
  static function_instance *
  read_function_instance (function_instance_stack *stack,
                          gcov_type head_count);

  /* Recursively deallocate all callsites (nested function_instances).  */
  ~function_instance ();

  /* Accessors.  */
  int
  name () const
  {
    return name_;
  }
  gcov_type
  total_count () const
  {
    return total_count_;
  }
  gcov_type
  head_count () const
  {
    return head_count_;
  }

  /* Traverse callsites of the current function_instance to find one at the
     location of LINENO and callee name represented in DECL.  */
  function_instance *get_function_instance_by_decl (unsigned lineno,
                                                    tree decl) const;

  /* Store the profile info for LOC in INFO. Return TRUE if profile info
     is found.  */
  bool get_count_info (location_t loc, count_info *info) const;

  /* Read the inlined indirect call target profile for STMT and store it in
     MAP, return the total count for all inlined indirect calls.  */
  gcov_type find_icall_target_map (gcall *stmt, icall_target_map *map) const;

  /* Sum of counts that is used during annotation.  */
  gcov_type total_annotated_count () const;

  /* Mark LOC as annotated.  */
  void mark_annotated (location_t loc);

private:
  /* Callsite, represented as (decl_lineno, callee_function_name_index).  */
  typedef std::pair<unsigned, unsigned> callsite;

  /* Map from callsite to callee function_instance.  */
  typedef std::map<callsite, function_instance *> callsite_map;

  function_instance (unsigned name, gcov_type head_count)
      : name_ (name), total_count_ (0), head_count_ (head_count)
  {
  }

  /* Map from source location (decl_lineno) to profile (count_info).  */
  typedef std::map<unsigned, count_info> position_count_map;

  /* function_instance name index in the string_table.  */
  unsigned name_;

  /* Total sample count.  */
  gcov_type total_count_;

  /* Entry BB's sample count.  */
  gcov_type head_count_;

  /* Map from callsite location to callee function_instance.  */
  callsite_map callsites;

  /* Map from source location to count_info.  */
  position_count_map pos_counts;
};

/* Profile for all functions.  */
class autofdo_source_profile
{
public:
  static autofdo_source_profile *
  create ()
  {
    autofdo_source_profile *map = new autofdo_source_profile ();

    if (map->read ())
      return map;
    delete map;
    return NULL;
  }

  ~autofdo_source_profile ();

  /* For a given DECL, returns the top-level function_instance.  */
  function_instance *get_function_instance_by_decl (tree decl) const;

  /* Find count_info for a given gimple STMT. If found, store the count_info
     in INFO and return true; otherwise return false.  */
  bool get_count_info (gimple *stmt, count_info *info) const;

  /* Find total count of the callee of EDGE.  */
  gcov_type get_callsite_total_count (struct cgraph_edge *edge) const;

  /* Update value profile INFO for STMT from the inlined indirect callsite.
     Return true if INFO is updated.  */
  bool update_inlined_ind_target (gcall *stmt, count_info *info);

  /* Mark LOC as annotated.  */
  void mark_annotated (location_t loc);

  /* Compute total count threshold of top functions in sampled data.  */
  gcov_type calc_topn_function_total_count_thres (unsigned topn) const;

private:
  /* Map from function_instance name index (in string_table) to
     function_instance.  */
  typedef std::map<unsigned, function_instance *> name_function_instance_map;

  autofdo_source_profile () {}

  /* Read AutoFDO profile and returns TRUE on success.  */
  bool read ();

  /* Return the function_instance in the profile that correspond to the
     inline STACK.  */
  function_instance *
  get_function_instance_by_inline_stack (const inline_stack &stack) const;

  name_function_instance_map map_;
};

/* Store the strings read from the profile data file.  */
static string_table *afdo_string_table;

/* Store the AutoFDO source profile.  */
static autofdo_source_profile *afdo_source_profile;

/* gcov_summary structure to store the profile_info.  */
static gcov_summary *afdo_profile_info;

/* Check opts->x_flags and put file name into EVENT_FILES.  */

static bool
get_all_profile_names (const char **event_files)
{
  if (!(flag_auto_profile
        || (flag_cache_misses_profile || flag_additional_profile)))
    {
      return false;
    }

  event_files[INST_EXEC] = auto_profile_file;

  if (flag_cache_misses_profile)
    {
      if (cache_misses_profile_file == NULL)
        {
          if (additional_profile_file == NULL)
        {
          additional_profile_file = DEFAULT_ADDITIONAL_PROFILE_FILE;
        }
      event_files[PMU_EVENT] = additional_profile_file;
        }
      event_files[CACHE_MISSES] = cache_misses_profile_file;
    }
  else if (flag_additional_profile)
    {
      if (additional_profile_file == NULL)
        {
          additional_profile_file = DEFAULT_ADDITIONAL_PROFILE_FILE;
        }
      event_files[PMU_EVENT] = additional_profile_file;
    }

  return true;
}

static void read_profile (void);

/* Maintain multiple profile data of different events with event_loc_count_map
   and event_func_count_map.  */

class extend_auto_profile
{
public:
  bool auto_profile_exist (enum event_type type);
  gcov_type get_loc_count (location_t, event_type);
  gcov_type get_func_count (unsigned, event_type);
  gcov_type get_topn_function_total_count_thres () const;
  struct rank_info get_func_rank (unsigned, enum event_type);
  /* There should be only one instance of class EXTEND_AUTO_PROFILE.  */
  static extend_auto_profile *create ()
    {
      extend_auto_profile *map = new extend_auto_profile ();
      if (map->read ())
	{
	  return map;
	}
      delete map;
      return NULL;
    }
private:
  /* Basic maps of extend_auto_profile.  */
  typedef std::map<location_t, gcov_type> loc_count_map;
  typedef std::map<unsigned, gcov_type> func_count_map;

  /* Map of function_uid to its descending order rank of counts.  */
  typedef std::map<unsigned, unsigned> rank_map;

  /* Mapping hardware events to corresponding basic maps.  */
  typedef std::map<event_type, loc_count_map> event_loc_count_map;
  typedef std::map<event_type, func_count_map> event_func_count_map;
  typedef std::map<event_type, rank_map> event_rank_map;

  extend_auto_profile () {}
  bool read ();
  void set_loc_count ();
  void process_extend_source_profile ();
  void read_extend_afdo_file (const char*, event_type);
  void rank_all_func ();
  void dump_event ();
  event_loc_count_map event_loc_map;
  event_func_count_map event_func_map;
  event_rank_map func_rank;
  event_type profile_type;
  gcov_type topn_function_total_count_thres;
};

/* Member functions for extend_auto_profile.  */

bool
extend_auto_profile::auto_profile_exist (enum event_type type)
{
  switch (type)
    {
      case INST_EXEC:
	return event_func_map.count (INST_EXEC) != 0
	       || event_loc_map.count (INST_EXEC) != 0;
      case CACHE_MISSES:
	return event_func_map.count (CACHE_MISSES) != 0
	       || event_loc_map.count (CACHE_MISSES) != 0;
      case PMU_EVENT:
	return event_func_map.count (PMU_EVENT) != 0
	       || event_loc_map.count (PMU_EVENT) != 0;
      default:
	  return false;
    }
}

void
extend_auto_profile::dump_event ()
{
  if (dump_file)
    {
      switch (profile_type)
	{
	  case INST_EXEC:
	    fprintf (dump_file, "Processing event instruction execution.\n");
	    break;
	  case CACHE_MISSES:
	    fprintf (dump_file, "Processing event cache misses.\n");
	    break;
        case PMU_EVENT:
	    fprintf (dump_file, "Processing other PMU events.\n");
	    break;
	  default:
	    break;
	}
    }
}

/* Return true if any profile data was read.  */

bool
extend_auto_profile::read ()
{
  const char *event_files[EVENT_NUMBER] = {NULL};
  if (!get_all_profile_names (event_files))
    {
      return false;
    }

  /* Backup AFDO_STRING_TABLE and AFDO_SOURCE_PROFILE since we will create
     new ones for each event_type.  */
  autofdo::string_table *string_table_afdo = afdo_string_table;
  autofdo::autofdo_source_profile *source_profile_afdo = afdo_source_profile;

  for (unsigned i = 0; i < EVENT_NUMBER; i++)
    {
      if (event_files[i] == NULL)
	{
	  continue;
	}
      profile_type = (enum event_type) i;
      dump_event ();
      gcov_close ();
      auto_profile_file = event_files[i];
      read_profile ();
      gcov_close ();

      topn_function_total_count_thres = param_llc_allocate_func_counts_threshold;
      if (param_llc_allocate_func_topn > 0 && profile_type == PMU_EVENT)
        {
	  topn_function_total_count_thres
	    = afdo_source_profile->calc_topn_function_total_count_thres (
		param_llc_allocate_func_topn);
        }

      process_extend_source_profile ();

      delete afdo_source_profile;
      delete afdo_string_table;
    }

  /* Restore AFDO_STRING_TABLE and AFDO_SOURCE_PROFILE.  Function
     END_AUTO_PROFILE will free them at the end of compilation.  */
  afdo_string_table = string_table_afdo;
  afdo_source_profile = source_profile_afdo;
  return true;
}

/* Helper functions.  */

gcov_type
extend_auto_profile::get_loc_count (location_t loc, event_type type)
{
  event_loc_count_map::iterator event_iter = event_loc_map.find (type);
  if (event_iter != event_loc_map.end ())
    {
      loc_count_map::iterator loc_iter = event_iter->second.find (loc);
      if (loc_iter != event_iter->second.end ())
	{
	  return loc_iter->second;
	}
    }
  return 0;
}

struct rank_info
extend_auto_profile::get_func_rank (unsigned decl_uid, enum event_type type)
{
  struct rank_info info = {0, 0};
  event_rank_map::iterator event_iter = func_rank.find (type);
  if (event_iter != func_rank.end ())
    {
      rank_map::iterator func_iter = event_iter->second.find (decl_uid);
      if (func_iter != event_iter->second.end ())
	{
	  info.rank = func_iter->second;
	  info.total = event_iter->second.size ();
	}
    }
  return info;
}

gcov_type
extend_auto_profile::get_func_count (unsigned decl_uid, event_type type)
{
  event_func_count_map::iterator event_iter = event_func_map.find (type);
  if (event_iter != event_func_map.end ())
    {
      func_count_map::iterator func_iter = event_iter->second.find (decl_uid);
      if (func_iter != event_iter->second.end ())
	{
	  return func_iter->second;
	}
    }
  return 0;
}

gcov_type
extend_auto_profile::get_topn_function_total_count_thres () const
{
  return topn_function_total_count_thres;
}

static extend_auto_profile *extend_profile;

/* Helper functions.  */

/* Return the original name of NAME: strip the suffix that starts
   with '.' Caller is responsible for freeing RET.  */

static char *
get_original_name (const char *name)
{
  char *ret = xstrdup (name);
  char *find = strchr (ret, '.');
  if (find != NULL)
    *find = 0;
  return ret;
}

/* Return the combined location, which is a 32bit integer in which
   higher 16 bits stores the line offset of LOC to the start lineno
   of DECL, The lower 16 bits stores the discriminator.  */

static unsigned
get_combined_location (location_t loc, tree decl)
{
  /* TODO: allow more bits for line and less bits for discriminator.  */
  if (LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl) >= (1<<16))
    warning_at (loc, OPT_Woverflow, "offset exceeds 16 bytes");
  return ((LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl)) << 16);
}

/* Return the function decl of a given lexical BLOCK.  */

static tree
get_function_decl_from_block (tree block)
{
  if (!inlined_function_outer_scope_p (block))
    return NULL_TREE;

  return BLOCK_ABSTRACT_ORIGIN (block);
}

/* Store inline stack for STMT in STACK.  */

static void
get_inline_stack (location_t locus, inline_stack *stack)
{
  if (LOCATION_LOCUS (locus) == UNKNOWN_LOCATION)
    return;

  tree block = LOCATION_BLOCK (locus);
  if (block && TREE_CODE (block) == BLOCK)
    {
      int level = 0;
      for (block = BLOCK_SUPERCONTEXT (block);
           block && (TREE_CODE (block) == BLOCK);
           block = BLOCK_SUPERCONTEXT (block))
        {
          location_t tmp_locus = BLOCK_SOURCE_LOCATION (block);
          if (LOCATION_LOCUS (tmp_locus) == UNKNOWN_LOCATION)
            continue;

          tree decl = get_function_decl_from_block (block);
          stack->safe_push (
              std::make_pair (decl, get_combined_location (locus, decl)));
          locus = tmp_locus;
          level++;
        }
    }
  stack->safe_push (
      std::make_pair (current_function_decl,
                      get_combined_location (locus, current_function_decl)));
}

/* Return STMT's combined location, which is a 32bit integer in which
   higher 16 bits stores the line offset of LOC to the start lineno
   of DECL, The lower 16 bits stores the discriminator.  */

static unsigned
get_relative_location_for_stmt (gimple *stmt)
{
  location_t locus = gimple_location (stmt);
  if (LOCATION_LOCUS (locus) == UNKNOWN_LOCATION)
    return UNKNOWN_LOCATION;

  for (tree block = gimple_block (stmt); block && (TREE_CODE (block) == BLOCK);
       block = BLOCK_SUPERCONTEXT (block))
    if (LOCATION_LOCUS (BLOCK_SOURCE_LOCATION (block)) != UNKNOWN_LOCATION)
      return get_combined_location (locus,
                                    get_function_decl_from_block (block));
  return get_combined_location (locus, current_function_decl);
}

/* Return true if BB contains indirect call.  */

static bool
has_indirect_call (basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_code (stmt) == GIMPLE_CALL && !gimple_call_internal_p (stmt)
          && (gimple_call_fn (stmt) == NULL
              || TREE_CODE (gimple_call_fn (stmt)) != FUNCTION_DECL))
        return true;
    }
  return false;
}

/* Member functions for string_table.  */

/* Deconstructor.  */

string_table::~string_table ()
{
  for (unsigned i = 0; i < vector_.length (); i++)
    free (vector_[i]);
}


/* Return the index of a given function NAME. Return -1 if NAME is not
   found in string table.  */

int
string_table::get_index (const char *name) const
{
  if (name == NULL)
    return -1;
  string_index_map::const_iterator iter = map_.find (name);
  /* Function name may be duplicate.  Try to distinguish by the
     #file_name#function_name defined by the autofdo tool chain.  */
  if (iter == map_.end ())
    {
      char* file_name = get_original_name (lbasename (dump_base_name));
      char* file_func_name
	= concat ("#", file_name, "#", name, NULL);
      iter = map_.find (file_func_name);
      free (file_name);
      free (file_func_name);
    }
  if (iter == map_.end ())
    return -1;

  return iter->second;
}

/* Return the index of a given function DECL. Return -1 if DECL is not
   found in string table.  */

int
string_table::get_index_by_decl (tree decl) const
{
  char *name
      = get_original_name (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
  int ret = get_index (name);
  free (name);
  if (ret != -1)
    return ret;
  ret = get_index (lang_hooks.dwarf_name (decl, 0));
  if (ret != -1)
    return ret;
  if (DECL_FROM_INLINE (decl))
    return get_index_by_decl (DECL_ABSTRACT_ORIGIN (decl));

  return -1;
}

/* Return the function name of a given INDEX.  */

const char *
string_table::get_name (int index) const
{
  gcc_assert (index > 0 && index < (int)vector_.length ());
  return vector_[index];
}

/* Read the string table. Return TRUE if reading is successful.  */

bool
string_table::read ()
{
  if (gcov_read_unsigned () != GCOV_TAG_AFDO_FILE_NAMES)
    return false;
  /* Skip the length of the section.  */
  gcov_read_unsigned ();
  /* Read in the file name table.  */
  unsigned string_num = gcov_read_unsigned ();
  for (unsigned i = 0; i < string_num; i++)
    {
      vector_.safe_push (get_original_name (gcov_read_string ()));
      map_[vector_.last ()] = i;
    }
  return true;
}

/* Member functions for function_instance.  */

function_instance::~function_instance ()
{
  for (callsite_map::iterator iter = callsites.begin ();
       iter != callsites.end (); ++iter)
    delete iter->second;
}

/* Traverse callsites of the current function_instance to find one at the
   location of LINENO and callee name represented in DECL.  */

function_instance *
function_instance::get_function_instance_by_decl (unsigned lineno,
                                                  tree decl) const
{
  int func_name_idx = afdo_string_table->get_index_by_decl (decl);
  if (func_name_idx != -1)
    {
      callsite_map::const_iterator ret
          = callsites.find (std::make_pair (lineno, func_name_idx));
      if (ret != callsites.end ())
        return ret->second;
    }
  func_name_idx
      = afdo_string_table->get_index (lang_hooks.dwarf_name (decl, 0));
  if (func_name_idx != -1)
    {
      callsite_map::const_iterator ret
          = callsites.find (std::make_pair (lineno, func_name_idx));
      if (ret != callsites.end ())
        return ret->second;
    }
  if (DECL_FROM_INLINE (decl))
    return get_function_instance_by_decl (lineno, DECL_ABSTRACT_ORIGIN (decl));

  return NULL;
}

/* Store the profile info for LOC in INFO. Return TRUE if profile info
   is found.  */

bool
function_instance::get_count_info (location_t loc, count_info *info) const
{
  position_count_map::const_iterator iter = pos_counts.find (loc);
  if (iter == pos_counts.end ())
    return false;
  *info = iter->second;
  return true;
}

/* Mark LOC as annotated.  */

void
function_instance::mark_annotated (location_t loc)
{
  position_count_map::iterator iter = pos_counts.find (loc);
  if (iter == pos_counts.end ())
    return;
  iter->second.annotated = true;
}

/* Read the inlined indirect call target profile for STMT and store it in
   MAP, return the total count for all inlined indirect calls.  */

gcov_type
function_instance::find_icall_target_map (gcall *stmt,
                                          icall_target_map *map) const
{
  gcov_type ret = 0;
  unsigned stmt_offset = get_relative_location_for_stmt (stmt);

  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end (); ++iter)
    {
      unsigned callee = iter->second->name ();
      /* Check if callsite location match the stmt.  */
      if (iter->first.first != stmt_offset)
        continue;
      struct cgraph_node *node = cgraph_node::get_for_asmname (
          get_identifier (afdo_string_table->get_name (callee)));
      if (node == NULL)
        continue;
      (*map)[callee] = iter->second->total_count ();
      ret += iter->second->total_count ();
    }
  return ret;
}

/* Read the profile and create a function_instance with head count as
   HEAD_COUNT. Recursively read callsites to create nested function_instances
   too. STACK is used to track the recursive creation process.  */

/* function instance profile format:

   ENTRY_COUNT: 8 bytes
   NAME_INDEX: 4 bytes
   NUM_POS_COUNTS: 4 bytes
   NUM_CALLSITES: 4 byte
   POS_COUNT_1:
     POS_1_OFFSET: 4 bytes
     NUM_TARGETS: 4 bytes
     COUNT: 8 bytes
     TARGET_1:
       VALUE_PROFILE_TYPE: 4 bytes
       TARGET_IDX: 8 bytes
       COUNT: 8 bytes
     TARGET_2
     ...
     TARGET_n
   POS_COUNT_2
   ...
   POS_COUNT_N
   CALLSITE_1:
     CALLSITE_1_OFFSET: 4 bytes
     FUNCTION_INSTANCE_PROFILE (nested)
   CALLSITE_2
   ...
   CALLSITE_n.  */

function_instance *
function_instance::read_function_instance (function_instance_stack *stack,
                                           gcov_type head_count)
{
  unsigned name = gcov_read_unsigned ();
  unsigned num_pos_counts = gcov_read_unsigned ();
  unsigned num_callsites = gcov_read_unsigned ();
  function_instance *s = new function_instance (name, head_count);
  stack->safe_push (s);

  for (unsigned i = 0; i < num_pos_counts; i++)
    {
      unsigned offset = gcov_read_unsigned ();
      unsigned num_targets = gcov_read_unsigned ();
      gcov_type count = gcov_read_counter ();
      s->pos_counts[offset].count = count;
      for (unsigned j = 0; j < stack->length (); j++)
        (*stack)[j]->total_count_ += count;
      for (unsigned j = 0; j < num_targets; j++)
        {
          /* Only indirect call target histogram is supported now.  */
          gcov_read_unsigned ();
          gcov_type target_idx = gcov_read_counter ();
          s->pos_counts[offset].targets[target_idx] = gcov_read_counter ();
        }
    }
  for (unsigned i = 0; i < num_callsites; i++)
    {
      unsigned offset = gcov_read_unsigned ();
      function_instance *callee_function_instance
          = read_function_instance (stack, 0);
      s->callsites[std::make_pair (offset, callee_function_instance->name ())]
          = callee_function_instance;
    }
  stack->pop ();
  return s;
}

/* Sum of counts that is used during annotation.  */

gcov_type
function_instance::total_annotated_count () const
{
  gcov_type ret = 0;
  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end (); ++iter)
    ret += iter->second->total_annotated_count ();
  for (position_count_map::const_iterator iter = pos_counts.begin ();
       iter != pos_counts.end (); ++iter)
    if (iter->second.annotated)
      ret += iter->second.count;
  return ret;
}

/* Member functions for autofdo_source_profile.  */

autofdo_source_profile::~autofdo_source_profile ()
{
  for (name_function_instance_map::const_iterator iter = map_.begin ();
       iter != map_.end (); ++iter)
    delete iter->second;
}

/* For a given DECL, returns the top-level function_instance.  */

function_instance *
autofdo_source_profile::get_function_instance_by_decl (tree decl) const
{
  int index = afdo_string_table->get_index_by_decl (decl);
  if (index == -1)
    return NULL;
  name_function_instance_map::const_iterator ret = map_.find (index);
  return ret == map_.end () ? NULL : ret->second;
}

/* Find count_info for a given gimple STMT. If found, store the count_info
   in INFO and return true; otherwise return false.  */

bool
autofdo_source_profile::get_count_info (gimple *stmt, count_info *info) const
{
  if (LOCATION_LOCUS (gimple_location (stmt)) == cfun->function_end_locus)
    return false;

  inline_stack stack;
  get_inline_stack (gimple_location (stmt), &stack);
  if (stack.length () == 0)
    return false;
  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    return false;
  if (s->get_count_info (stack[0].second + stmt->bb->discriminator, info))
    {
      return true;
    }
  return s->get_count_info (stack[0].second, info);
}

/* Mark LOC as annotated.  */

void
autofdo_source_profile::mark_annotated (location_t loc)
{
  inline_stack stack;
  get_inline_stack (loc, &stack);
  if (stack.length () == 0)
    return;
  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    return;
  s->mark_annotated (stack[0].second);
}

/* Update value profile INFO for STMT from the inlined indirect callsite.
   Return true if INFO is updated.  */

bool
autofdo_source_profile::update_inlined_ind_target (gcall *stmt,
                                                   count_info *info)
{
  if (dump_file)
    {
      fprintf (dump_file, "Checking indirect call -> direct call ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  if (LOCATION_LOCUS (gimple_location (stmt)) == cfun->function_end_locus)
    {
      if (dump_file)
	fprintf (dump_file, " good locus\n");
      return false;
    }

  count_info old_info;
  get_count_info (stmt, &old_info);
  gcov_type total = 0;
  for (icall_target_map::const_iterator iter = old_info.targets.begin ();
       iter != old_info.targets.end (); ++iter)
    total += iter->second;

  /* Program behavior changed, original promoted (and inlined) target is not
     hot any more. Will avoid promote the original target.

     To check if original promoted target is still hot, we check the total
     count of the unpromoted targets (stored in TOTAL). If a callsite count
     (stored in INFO) is smaller than half of the total count, the original
     promoted target is considered not hot any more.  */
  if (info->count < total / 2)
    {
      if (dump_file)
	fprintf (dump_file, " not hot anymore %ld < %ld",
		 (long)info->count,
		 (long)total /2);
      return false;
    }

  inline_stack stack;
  get_inline_stack (gimple_location (stmt), &stack);
  if (stack.length () == 0)
    {
      if (dump_file)
	fprintf (dump_file, " no inline stack\n");
      return false;
    }
  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    {
      if (dump_file)
	fprintf (dump_file, " function not found in inline stack\n");
      return false;
    }
  icall_target_map map;
  if (s->find_icall_target_map (stmt, &map) == 0)
    {
      if (dump_file)
	fprintf (dump_file, " no target map\n");
      return false;
    }
  for (icall_target_map::const_iterator iter = map.begin ();
       iter != map.end (); ++iter)
    info->targets[iter->first] = iter->second;
  if (dump_file)
    fprintf (dump_file, " looks good\n");
  return true;
}

/* Find total count of the callee of EDGE.  */

gcov_type
autofdo_source_profile::get_callsite_total_count (
    struct cgraph_edge *edge) const
{
  inline_stack stack;
  stack.safe_push (std::make_pair (edge->callee->decl, 0));
  get_inline_stack (gimple_location (edge->call_stmt), &stack);

  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL
      || afdo_string_table->get_index (IDENTIFIER_POINTER (
             DECL_ASSEMBLER_NAME (edge->callee->decl))) != s->name ())
    return 0;

  return s->total_count ();
}

/* Read AutoFDO profile and returns TRUE on success.  */

/* source profile format:

   GCOV_TAG_AFDO_FUNCTION: 4 bytes
   LENGTH: 4 bytes
   NUM_FUNCTIONS: 4 bytes
   FUNCTION_INSTANCE_1
   FUNCTION_INSTANCE_2
   ...
   FUNCTION_INSTANCE_N.  */

bool
autofdo_source_profile::read ()
{
  if (gcov_read_unsigned () != GCOV_TAG_AFDO_FUNCTION)
    {
      inform (UNKNOWN_LOCATION, "Not expected TAG.");
      return false;
    }

  /* Skip the length of the section.  */
  gcov_read_unsigned ();

  /* Read in the function/callsite profile, and store it in local
     data structure.  */
  unsigned function_num = gcov_read_unsigned ();
  for (unsigned i = 0; i < function_num; i++)
    {
      function_instance::function_instance_stack stack;
      function_instance *s = function_instance::read_function_instance (
          &stack, gcov_read_counter ());
      map_[s->name ()] = s;
    }
  return true;
}

/* Return the function_instance in the profile that correspond to the
   inline STACK.  */

function_instance *
autofdo_source_profile::get_function_instance_by_inline_stack (
    const inline_stack &stack) const
{
  name_function_instance_map::const_iterator iter = map_.find (
      afdo_string_table->get_index_by_decl (stack[stack.length () - 1].first));
  if (iter == map_.end())
    return NULL;
  function_instance *s = iter->second;
  for (unsigned i = stack.length() - 1; i > 0; i--)
    {
      s = s->get_function_instance_by_decl (
          stack[i].second, stack[i - 1].first);
      if (s == NULL)
        return NULL;
    }
  return s;
}

/* Compute total count threshold of top functions in sampled data.  */

gcov_type
autofdo_source_profile::calc_topn_function_total_count_thres (
    unsigned topn) const
{
  std::set<gcov_type> func_counts;
  for (name_function_instance_map::const_iterator iter = map_.begin ();
       iter != map_.end (); ++iter)
    {
      if (func_counts.size () < topn)
        func_counts.insert (iter->second->total_count ());
      else if (*func_counts.begin () < iter->second->total_count ())
        {
          func_counts.erase (func_counts.begin ());
          func_counts.insert (iter->second->total_count ());
        }
    }
 
  gcov_type func_counts_topn = *func_counts.begin ();
  if (func_counts.size () == topn
      && param_llc_allocate_func_counts_threshold < func_counts_topn)
    return func_counts_topn;
}

/* Module profile is only used by LIPO. Here we simply ignore it.  */

static void
fake_read_autofdo_module_profile ()
{
  /* Read in the module info.  */
  gcov_read_unsigned ();

  /* Skip the length of the section.  */
  gcov_read_unsigned ();

  /* Read in the file name table.  */
  unsigned total_module_num = gcov_read_unsigned ();
  gcc_assert (total_module_num == 0);
}

/* Read data from profile data file.  */

static void
read_profile (void)
{
  if (gcov_open (auto_profile_file, 1) == 0)
    {
      error ("cannot open profile file %s", auto_profile_file);
      return;
    }

  if (gcov_read_unsigned () != GCOV_DATA_MAGIC)
    {
      error ("AutoFDO profile magic number does not match");
      return;
    }

  /* Skip the version number.  */
  unsigned version = gcov_read_unsigned ();
  if (version != AUTO_PROFILE_VERSION)
    {
      error ("AutoFDO profile version %u does not match %u",
	     version, AUTO_PROFILE_VERSION);
      return;
    }

  /* Skip the empty integer.  */
  gcov_read_unsigned ();

  /* string_table.  */
  afdo_string_table = new string_table ();
  if (!afdo_string_table->read())
    {
      error ("cannot read string table from %s", auto_profile_file);
      return;
    }

  /* autofdo_source_profile.  */
  afdo_source_profile = autofdo_source_profile::create ();
  if (afdo_source_profile == NULL)
    {
      error ("cannot read function profile from %s", auto_profile_file);
      return;
    }

  /* autofdo_module_profile.  */
  fake_read_autofdo_module_profile ();
}

/* From AutoFDO profiles, find values inside STMT for that we want to measure
   histograms for indirect-call optimization.

   This function is actually served for 2 purposes:
     * before annotation, we need to mark histogram, promote and inline
     * after annotation, we just need to mark, and let follow-up logic to
       decide if it needs to promote and inline.  */

static bool
afdo_indirect_call (gimple_stmt_iterator *gsi, const icall_target_map &map,
                    bool transform)
{
  gimple *gs = gsi_stmt (*gsi);
  tree callee;

  if (map.size () == 0)
    return false;
  gcall *stmt = dyn_cast <gcall *> (gs);
  if (!stmt
      || gimple_call_internal_p (stmt)
      || gimple_call_fndecl (stmt) != NULL_TREE)
    return false;

  gcov_type total = 0;
  icall_target_map::const_iterator max_iter = map.end ();

  for (icall_target_map::const_iterator iter = map.begin ();
       iter != map.end (); ++iter)
    {
      total += iter->second;
      if (max_iter == map.end () || max_iter->second < iter->second)
        max_iter = iter;
    }
  struct cgraph_node *direct_call = cgraph_node::get_for_asmname (
      get_identifier (afdo_string_table->get_name (max_iter->first)));
  if (direct_call == NULL || !direct_call->profile_id)
    return false;

  callee = gimple_call_fn (stmt);

  histogram_value hist = gimple_alloc_histogram_value (
      cfun, HIST_TYPE_INDIR_CALL, stmt, callee);
  hist->n_counters = 4;
  hist->hvalue.counters = XNEWVEC (gcov_type, hist->n_counters);
  gimple_add_histogram_value (cfun, stmt, hist);

  /* Total counter */
  hist->hvalue.counters[0] = total;
  /* Number of value/counter pairs */
  hist->hvalue.counters[1] = 1;
  /* Value */
  hist->hvalue.counters[2] = direct_call->profile_id;
  /* Counter */
  hist->hvalue.counters[3] = max_iter->second;

  if (!transform)
    return false;

  cgraph_node* current_function_node = cgraph_node::get (current_function_decl);

  /* If the direct call is a recursive call, don't promote it since
     we are not set up to inline recursive calls at this stage. */
  if (direct_call == current_function_node)
    return false;

  struct cgraph_edge *indirect_edge
      = current_function_node->get_edge (stmt);

  if (dump_file)
    {
      fprintf (dump_file, "Indirect call -> direct call ");
      print_generic_expr (dump_file, callee, TDF_SLIM);
      fprintf (dump_file, " => ");
      print_generic_expr (dump_file, direct_call->decl, TDF_SLIM);
    }

  if (direct_call == NULL)
    {
      if (dump_file)
        fprintf (dump_file, " not transforming\n");
      return false;
    }
  if (DECL_STRUCT_FUNCTION (direct_call->decl) == NULL)
    {
      if (dump_file)
        fprintf (dump_file, " no declaration\n");
      return false;
    }

  if (dump_file)
    {
      fprintf (dump_file, " transformation on insn ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  /* FIXME: Count should be initialized.  */
  struct cgraph_edge *new_edge
      = indirect_edge->make_speculative (direct_call,
					 profile_count::uninitialized ());
  cgraph_edge::redirect_call_stmt_to_callee (new_edge);
  gimple_remove_histogram_value (cfun, stmt, hist);
  inline_call (new_edge, true, NULL, NULL, false);
  return true;
}

/* From AutoFDO profiles, find values inside STMT for that we want to measure
   histograms and adds them to list VALUES.  */

static bool
afdo_vpt (gimple_stmt_iterator *gsi, const icall_target_map &map,
          bool transform)
{
  return afdo_indirect_call (gsi, map, transform);
}

typedef std::set<basic_block> bb_set;
typedef std::set<edge> edge_set;

static bool
is_bb_annotated (const basic_block bb, const bb_set &annotated)
{
  return annotated.find (bb) != annotated.end ();
}

static void
set_bb_annotated (basic_block bb, bb_set *annotated)
{
  annotated->insert (bb);
}

/* For a given BB, set its execution count. Attach value profile if a stmt
   is not in PROMOTED, because we only want to promote an indirect call once.
   Return TRUE if BB is annotated.  */

static bool
afdo_set_bb_count (basic_block bb, const stmt_set &promoted)
{
  gimple_stmt_iterator gsi;
  edge e;
  edge_iterator ei;
  gcov_type max_count = 0;
  bool has_annotated = false;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      count_info info;
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_clobber_p (stmt) || is_gimple_debug (stmt))
        continue;
      if (afdo_source_profile->get_count_info (stmt, &info))
        {
          if (info.count > max_count)
            max_count = info.count;
          has_annotated = true;
          if (info.targets.size () > 0
              && promoted.find (stmt) == promoted.end ())
            afdo_vpt (&gsi, info.targets, false);
        }
    }

  if (!has_annotated)
    return false;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    afdo_source_profile->mark_annotated (gimple_location (gsi_stmt (gsi)));
  for (gphi_iterator gpi = gsi_start_phis (bb);
       !gsi_end_p (gpi);
       gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      size_t i;
      for (i = 0; i < gimple_phi_num_args (phi); i++)
        afdo_source_profile->mark_annotated (gimple_phi_arg_location (phi, i));
    }
  FOR_EACH_EDGE (e, ei, bb->succs)
  afdo_source_profile->mark_annotated (e->goto_locus);

  bb->count = profile_count::from_gcov_type (max_count).afdo ();
  return true;
}

/* BB1 and BB2 are in an equivalent class iff:
   1. BB1 dominates BB2.
   2. BB2 post-dominates BB1.
   3. BB1 and BB2 are in the same loop nest.
   This function finds the equivalent class for each basic block, and
   stores a pointer to the first BB in its equivalent class. Meanwhile,
   set bb counts for the same equivalent class to be idenical. Update
   ANNOTATED_BB for the first BB in its equivalent class.  */

static void
afdo_find_equiv_class (bb_set *annotated_bb)
{
  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
  bb->aux = NULL;

  FOR_ALL_BB_FN (bb, cfun)
  {
    if (bb->aux != NULL)
      continue;
    bb->aux = bb;
    for (basic_block bb1 : get_dominated_by (CDI_DOMINATORS, bb))
      if (bb1->aux == NULL && dominated_by_p (CDI_POST_DOMINATORS, bb, bb1)
	  && bb1->loop_father == bb->loop_father)
	{
	  bb1->aux = bb;
	  if (bb1->count > bb->count && is_bb_annotated (bb1, *annotated_bb))
	    {
	      bb->count = bb1->count;
	      set_bb_annotated (bb, annotated_bb);
	    }
	}

    for (basic_block bb1 : get_dominated_by (CDI_POST_DOMINATORS, bb))
      if (bb1->aux == NULL && dominated_by_p (CDI_DOMINATORS, bb, bb1)
	  && bb1->loop_father == bb->loop_father)
	{
	  bb1->aux = bb;
	  if (bb1->count > bb->count && is_bb_annotated (bb1, *annotated_bb))
	    {
	      bb->count = bb1->count;
	      set_bb_annotated (bb, annotated_bb);
	    }
	}
  }
}

/* If a basic block's count is known, and only one of its in/out edges' count
   is unknown, its count can be calculated. Meanwhile, if all of the in/out
   edges' counts are known, then the basic block's unknown count can also be
   calculated. Also, if a block has a single predecessor or successor, the block's
   count can be propagated to that predecessor or successor.
   IS_SUCC is true if out edges of a basic blocks are examined.
   Update ANNOTATED_BB accordingly.
   Return TRUE if any basic block/edge count is changed.  */

static bool
afdo_propagate_edge (bool is_succ, bb_set *annotated_bb)
{
  basic_block bb;
  bool changed = false;

  FOR_EACH_BB_FN (bb, cfun)
  {
    edge e, unknown_edge = NULL;
    edge_iterator ei;
    int num_unknown_edge = 0;
    int num_edge = 0;
    profile_count total_known_count = profile_count::zero ().afdo ();

    FOR_EACH_EDGE (e, ei, is_succ ? bb->succs : bb->preds)
      {
	gcc_assert (AFDO_EINFO (e) != NULL);
	if (! AFDO_EINFO (e)->is_annotated ())
	  num_unknown_edge++, unknown_edge = e;
	else
	  total_known_count += AFDO_EINFO (e)->get_count ();
	num_edge++;
      }

    /* Be careful not to annotate block with no successor in special cases.  */
    if (num_unknown_edge == 0 && total_known_count > bb->count)
      {
	bb->count = total_known_count;
	if (!is_bb_annotated (bb, *annotated_bb))
	  set_bb_annotated (bb, annotated_bb);
	changed = true;
      }
    else if (num_unknown_edge == 1 && is_bb_annotated (bb, *annotated_bb))
      {
	if (bb->count > total_known_count)
	  {
	      profile_count new_count = bb->count - total_known_count;
	      AFDO_EINFO(unknown_edge)->set_count(new_count);
	      if (num_edge == 1)
		{
		  basic_block succ_or_pred_bb = is_succ ? unknown_edge->dest : unknown_edge->src;
		  if (new_count > succ_or_pred_bb->count)
		    {
		      succ_or_pred_bb->count = new_count;
		      if (!is_bb_annotated (succ_or_pred_bb, *annotated_bb))
			set_bb_annotated (succ_or_pred_bb, annotated_bb);
		    }
		}
	   }
	else
	  AFDO_EINFO (unknown_edge)->set_count (profile_count::zero().afdo ());
	AFDO_EINFO (unknown_edge)->set_annotated ();
	changed = true;
      }
  }
  return changed;
}

/* Special propagation for circuit expressions. Because GCC translates
   control flow into data flow for circuit expressions. E.g.
   BB1:
   if (a && b)
     BB2
   else
     BB3

   will be translated into:

   BB1:
     if (a)
       goto BB.t1
     else
       goto BB.t3
   BB.t1:
     if (b)
       goto BB.t2
     else
       goto BB.t3
   BB.t2:
     goto BB.t3
   BB.t3:
     tmp = PHI (0 (BB1), 0 (BB.t1), 1 (BB.t2)
     if (tmp)
       goto BB2
     else
       goto BB3

   In this case, we need to propagate through PHI to determine the edge
   count of BB1->BB.t1, BB.t1->BB.t2.  */

static void
afdo_propagate_circuit (const bb_set &annotated_bb)
{
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
  {
    gimple *def_stmt;
    tree cmp_rhs, cmp_lhs;
    gimple *cmp_stmt = last_stmt (bb);
    edge e;
    edge_iterator ei;

    if (!cmp_stmt || gimple_code (cmp_stmt) != GIMPLE_COND)
      continue;
    cmp_rhs = gimple_cond_rhs (cmp_stmt);
    cmp_lhs = gimple_cond_lhs (cmp_stmt);
    if (!TREE_CONSTANT (cmp_rhs)
        || !(integer_zerop (cmp_rhs) || integer_onep (cmp_rhs)))
      continue;
    if (TREE_CODE (cmp_lhs) != SSA_NAME)
      continue;
    if (!is_bb_annotated (bb, annotated_bb))
      continue;
    def_stmt = SSA_NAME_DEF_STMT (cmp_lhs);
    while (def_stmt && gimple_code (def_stmt) == GIMPLE_ASSIGN
           && gimple_assign_single_p (def_stmt)
           && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME)
      def_stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (def_stmt));
    if (!def_stmt)
      continue;
    gphi *phi_stmt = dyn_cast <gphi *> (def_stmt);
    if (!phi_stmt)
      continue;
    FOR_EACH_EDGE (e, ei, bb->succs)
    {
      unsigned i, total = 0;
      edge only_one;
      bool check_value_one = (((integer_onep (cmp_rhs))
                               ^ (gimple_cond_code (cmp_stmt) == EQ_EXPR))
                              ^ ((e->flags & EDGE_TRUE_VALUE) != 0));
      if (! AFDO_EINFO (e)->is_annotated ())
        continue;
      for (i = 0; i < gimple_phi_num_args (phi_stmt); i++)
        {
          tree val = gimple_phi_arg_def (phi_stmt, i);
          edge ep = gimple_phi_arg_edge (phi_stmt, i);

          if (!TREE_CONSTANT (val)
              || !(integer_zerop (val) || integer_onep (val)))
            continue;
          if (check_value_one ^ integer_onep (val))
            continue;
          total++;
          only_one = ep;
          if (! (AFDO_EINFO (e)->get_count ()).nonzero_p ()
	      && ! AFDO_EINFO (ep)->is_annotated ())
	    {
	      AFDO_EINFO (ep)->set_count (profile_count::zero ().afdo ());
	      AFDO_EINFO (ep)->set_annotated ();
	    }
	}
      if (total == 1 && ! AFDO_EINFO (only_one)->is_annotated ())
	{
	  AFDO_EINFO (only_one)->set_count (AFDO_EINFO (e)->get_count ());
	  AFDO_EINFO (only_one)->set_annotated ();
	}
    }
  }
}

/* Propagate the basic block count and edge count on the control flow
   graph. We do the propagation iteratively until stablize.  */

static void
afdo_propagate (bb_set *annotated_bb)
{
  basic_block bb;
  bool changed = true;
  int i = 0;

  FOR_ALL_BB_FN (bb, cfun)
  {
    bb->count = ((basic_block)bb->aux)->count;
    if (is_bb_annotated ((basic_block)bb->aux, *annotated_bb))
      set_bb_annotated (bb, annotated_bb);
  }

  while (changed && i++ < 10)
    {
      changed = false;

      if (afdo_propagate_edge (true, annotated_bb))
        changed = true;
      if (afdo_propagate_edge (false, annotated_bb))
        changed = true;
      afdo_propagate_circuit (*annotated_bb);
    }
}

/* Process the following scene when the branch probability
   inversion when do function afdo_propagate (). E.g.
   BB_NUM (sample count)
      BB1 (1000)
       /    \
    BB2 (10) BB3 (0)
      \       /
	BB4
   In afdo_propagate ().count of BB3 is calculated by
   COUNT (BB3) = 990 (990 = COUNT (BB1) - COUNT (BB2) = 1000 - 10)
   In fact, BB3 may be colder than BB2 by sample count.
   This function allocate source BB count to wach succ BB by sample
   rate, E.g.
   BB2_COUNT = BB1_COUNT * (BB2_COUNT / (BB2_COUNT + BB3_COUNT))  */

static void
afdo_preprocess_bb_count ()
{
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    {
      if (bb->count.ipa_p () && EDGE_COUNT (bb->succs) > 1
	  && bb->count > profile_count::zero ().afdo ())
	{
	  basic_block bb1 = EDGE_SUCC (bb, 0)->dest;
	  basic_block bb2 = EDGE_SUCC (bb, 1)->dest;
	  if (single_succ_edge (bb1) && single_succ_edge (bb2)
	      && EDGE_SUCC (bb1, 0)->dest == EDGE_SUCC (bb2, 0)->dest)
	    {
	      gcov_type max_count = 0;
	      gcov_type total_count = 0;
	      edge e;
	      edge_iterator ei;
	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  if (!e->dest->count.ipa_p ())
		    {
		      continue;
		    }
		  max_count = MAX (max_count, e->dest->count.to_gcov_type ());
		  total_count += e->dest->count.to_gcov_type ();
		}
	      /* Only bb_count > max_count * 2, branch probability will
		 inversion.  */
	      if (max_count > 0 && bb->count.to_gcov_type () > max_count * 2)
		{
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    {
		      gcov_type target_count = bb->count.to_gcov_type ()
			* e->dest->count.to_gcov_type ()/ total_count;
		      e->dest->count
			= profile_count::from_gcov_type
			  (target_count).afdo ();
		    }
		}
	    }
	}
    }
}

/* Propagate counts on control flow graph and calculate branch
   probabilities.  */

static void
afdo_calculate_branch_prob (bb_set *annotated_bb)
{
  edge e;
  edge_iterator ei;
  basic_block bb;

  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  loop_optimizer_init (0);

  FOR_ALL_BB_FN (bb, cfun)
    {
      gcc_assert (bb->aux == NULL);
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  gcc_assert (e->aux == NULL);
	  e->aux = new edge_info ();
	}
    }

  afdo_find_equiv_class (annotated_bb);
  afdo_preprocess_bb_count ();
  afdo_propagate (annotated_bb);

  FOR_EACH_BB_FN (bb, cfun)
  {
    int num_unknown_succ = 0;
    profile_count total_count = profile_count::zero ().afdo ();

    FOR_EACH_EDGE (e, ei, bb->succs)
    {
      gcc_assert (AFDO_EINFO (e) != NULL);
      if (! AFDO_EINFO (e)->is_annotated ())
        num_unknown_succ++;
      else
        total_count += AFDO_EINFO (e)->get_count ();
    }
    if (num_unknown_succ == 0 && total_count > profile_count::zero ())
      {
	FOR_EACH_EDGE (e, ei, bb->succs)
	  e->probability
	    = AFDO_EINFO (e)->get_count ().probability_in (total_count);
      }
  }
  FOR_ALL_BB_FN (bb, cfun)
    {
      bb->aux = NULL;
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (AFDO_EINFO (e) != NULL)
	  {
	    delete AFDO_EINFO (e);
	    e->aux = NULL;
	  }
    }

  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
}

/* Perform value profile transformation using AutoFDO profile. Add the
   promoted stmts to PROMOTED_STMTS. Return TRUE if there is any
   indirect call promoted.  */

static bool
afdo_vpt_for_early_inline (stmt_set *promoted_stmts)
{
  basic_block bb;
  if (afdo_source_profile->get_function_instance_by_decl (
          current_function_decl) == NULL)
    return false;

  compute_fn_summary (cgraph_node::get (current_function_decl), true);

  bool has_vpt = false;
  FOR_EACH_BB_FN (bb, cfun)
  {
    if (!has_indirect_call (bb))
      continue;
    gimple_stmt_iterator gsi;

    gcov_type bb_count = 0;
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
        count_info info;
	gimple *stmt = gsi_stmt (gsi);
        if (afdo_source_profile->get_count_info (stmt, &info))
          bb_count = MAX (bb_count, info.count);
      }

    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
        gcall *stmt = dyn_cast <gcall *> (gsi_stmt (gsi));
        /* IC_promotion and early_inline_2 is done in multiple iterations.
           No need to promoted the stmt if its in promoted_stmts (means
           it is already been promoted in the previous iterations).  */
        if ((!stmt) || gimple_call_fn (stmt) == NULL
            || TREE_CODE (gimple_call_fn (stmt)) == FUNCTION_DECL
            || promoted_stmts->find (stmt) != promoted_stmts->end ())
          continue;

        count_info info;
        afdo_source_profile->get_count_info (stmt, &info);
        info.count = bb_count;
        if (afdo_source_profile->update_inlined_ind_target (stmt, &info))
          {
            /* Promote the indirect call and update the promoted_stmts.  */
            promoted_stmts->insert (stmt);
            if (afdo_vpt (&gsi, info.targets, true))
              has_vpt = true;
          }
      }
  }

  if (has_vpt)
    {
      unsigned todo = optimize_inline_calls (current_function_decl);
      if (todo & TODO_update_ssa_any)
       update_ssa (TODO_update_ssa);
      return true;
    }

  return false;
}

/* Preparation before executing MCF algorithm.  */

static void
afdo_init_mcf ()
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  if (dump_file)
    {
      fprintf (dump_file, "\n init calling mcf_smooth_cfg (). \n");
    }

  /* Step1: when use mcf, BB id must be continous,
     so we need compact_blocks ().  */
  compact_blocks ();

  /* Step2: allocate memory for MCF input data.  */
  bb_gcov_counts.safe_grow_cleared (cfun->cfg->x_last_basic_block);
  edge_gcov_counts = new hash_map<edge, gcov_type>;

  /* Step3: init MCF input data from cfg.  */
  FOR_ALL_BB_FN (bb, cfun)
    {
      /* Init BB count for MCF.  */
      bb_gcov_count (bb) = bb->count.to_gcov_type ();

      gcov_type total_count = 0;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  total_count += e->dest->count.to_gcov_type ();
	}

      /* If there is no sample in each successor blocks, source
	 BB samples are allocated to each edge by branch static prob.  */

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (total_count == 0)
	    {
	      edge_gcov_count (e) = e->src->count.to_gcov_type ()
		* e->probability.to_reg_br_prob_base () / REG_BR_PROB_BASE;
	    }
	  else
	    {
	      edge_gcov_count (e) = e->src->count.to_gcov_type ()
		* e->dest->count.to_gcov_type () / total_count;
	    }
	}
    }
}


/* Free the resources used by MCF and reset BB count from MCF result.
   branch probability has been updated in mcf_smooth_cfg ().  */

static void
afdo_process_after_mcf ()
{
  basic_block bb;
  /* Reset BB count from MCF result.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (bb_gcov_count (bb))
	{
	  bb->count
	    = profile_count::from_gcov_type (bb_gcov_count (bb)).afdo ();
	}
    }

    /* Clean up MCF resource.  */
    bb_gcov_counts.release ();
    delete edge_gcov_counts;
    edge_gcov_counts = NULL;
}

/* Annotate auto profile to the control flow graph. Do not annotate value
   profile for stmts in PROMOTED_STMTS.  */

static void
afdo_annotate_cfg (const stmt_set &promoted_stmts)
{
  basic_block bb;
  bb_set annotated_bb;
  const function_instance *s
      = afdo_source_profile->get_function_instance_by_decl (
          current_function_decl);

  if (s == NULL)
    return;
  cgraph_node::get (current_function_decl)->count
     = profile_count::from_gcov_type (s->head_count ()).afdo ();
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->count
     = profile_count::from_gcov_type (s->head_count ()).afdo ();
  EXIT_BLOCK_PTR_FOR_FN (cfun)->count = profile_count::zero ().afdo ();
  profile_count max_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;

  FOR_EACH_BB_FN (bb, cfun)
    {
      /* As autoFDO uses sampling approach, we have to assume that all
	 counters are zero when not seen by autoFDO.  */
      bb->count = profile_count::zero ().afdo ();
      if (afdo_set_bb_count (bb, promoted_stmts))
	set_bb_annotated (bb, &annotated_bb);
      if (bb->count > max_count)
	max_count = bb->count;
    }
  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count
      > ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->count)
    {
      ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->count
          = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      set_bb_annotated (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb, &annotated_bb);
    }
  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count
      > EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb->count)
    {
      EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb->count
          = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      set_bb_annotated (EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb, &annotated_bb);
    }
  afdo_source_profile->mark_annotated (
      DECL_SOURCE_LOCATION (current_function_decl));
  afdo_source_profile->mark_annotated (cfun->function_start_locus);
  afdo_source_profile->mark_annotated (cfun->function_end_locus);
  if (max_count > profile_count::zero ())
    {
      /* 1 means -fprofile-correction is enbaled manually, and MCF
	 algorithm will be used to calculate count and probability.
	 Otherwise, use the default calculate algorithm.  */
      if (flag_profile_correction == 1)
	{
	  afdo_init_mcf ();
	  mcf_smooth_cfg ();
	  afdo_process_after_mcf ();
	}
      else
	{
	  /* Calculate, propagate count and probability information on CFG.  */
	  afdo_calculate_branch_prob (&annotated_bb);
	}
    }
  update_max_bb_count ();
  profile_status_for_fn (cfun) = PROFILE_READ;
  if (flag_value_profile_transformations)
    {
      gimple_value_profile_transformations ();
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      update_ssa (TODO_update_ssa);
    }
}

/* Wrapper function to invoke early inliner.  */

static void
early_inline ()
{
  compute_fn_summary (cgraph_node::get (current_function_decl), true);
  unsigned todo = early_inliner (cfun);
  if (todo & TODO_update_ssa_any)
    update_ssa (TODO_update_ssa);
}

/* Use AutoFDO profile to annoate the control flow graph.
   Return the todo flag.  */

static unsigned int
auto_profile (void)
{
  struct cgraph_node *node;

  if (symtab->state == FINISHED)
    return 0;

  init_node_map (true, false);
  profile_info = autofdo::afdo_profile_info;

  FOR_EACH_FUNCTION (node)
  {
    if (!gimple_has_body_p (node->decl))
      continue;

    /* Don't profile functions produced for builtin stuff.  */
    if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
      continue;

    push_cfun (DECL_STRUCT_FUNCTION (node->decl));

    /* First do indirect call promotion and early inline to make the
       IR match the profiled binary before actual annotation.

       This is needed because an indirect call might have been promoted
       and inlined in the profiled binary. If we do not promote and
       inline these indirect calls before annotation, the profile for
       these promoted functions will be lost.

       e.g. foo() --indirect_call--> bar()
       In profiled binary, the callsite is promoted and inlined, making
       the profile look like:

       foo: {
         loc_foo_1: count_1
         bar@loc_foo_2: {
           loc_bar_1: count_2
           loc_bar_2: count_3
         }
       }

       Before AutoFDO pass, loc_foo_2 is not promoted thus not inlined.
       If we perform annotation on it, the profile inside bar@loc_foo2
       will be wasted.

       To avoid this, we promote loc_foo_2 and inline the promoted bar
       function before annotation, so the profile inside bar@loc_foo2
       will be useful.  */
    autofdo::stmt_set promoted_stmts;
    for (int i = 0; i < 10; i++)
      {
        if (!flag_value_profile_transformations
            || !autofdo::afdo_vpt_for_early_inline (&promoted_stmts))
          break;
        early_inline ();
      }

    early_inline ();
    autofdo::afdo_annotate_cfg (promoted_stmts);
    compute_function_frequency ();

    /* Local pure-const may imply need to fixup the cfg.  */
    if (execute_fixup_cfg () & TODO_cleanup_cfg)
      cleanup_tree_cfg ();

    free_dominance_info (CDI_DOMINATORS);
    free_dominance_info (CDI_POST_DOMINATORS);
    cgraph_edge::rebuild_edges ();
    compute_fn_summary (cgraph_node::get (current_function_decl), true);
    pop_cfun ();
  }

  return TODO_rebuild_cgraph_edges;
}


void
extend_auto_profile::rank_all_func ()
{
  std::vector<std::pair<unsigned, gcov_type> > func_sorted;
  event_func_count_map::iterator event_iter
				 = event_func_map.find (profile_type);
  if (event_iter != event_func_map.end ())
    {
      func_count_map::iterator func_iter;
      for (func_iter = event_iter->second.begin ();
	   func_iter != event_iter->second.end (); func_iter++)
	{
	  func_sorted.push_back (std::make_pair (func_iter->first,
						 func_iter->second));
	}

      std::sort (func_sorted.begin (), func_sorted.end (), event_count_cmp);

      for (unsigned i = 0; i < func_sorted.size (); ++i)
	{
	  func_rank[profile_type][func_sorted[i].first] = i + 1;
	}
    }
}

/* Iterate stmts in cfun and maintain its count to EVENT_LOC_MAP.  */

void
extend_auto_profile::set_loc_count ()
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  count_info info;
	  gimple *stmt = gsi_stmt (gsi);
	  if (gimple_clobber_p (stmt) || is_gimple_debug (stmt))
	    {
	      continue;
	    }
	  if (afdo_source_profile->get_count_info (stmt, &info))
	    {
	      location_t loc = gimple_location (stmt);
	      event_loc_map[profile_type][loc] += info.count;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "stmt ");
		  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
		  fprintf (dump_file, "counts %ld\n",
			   event_loc_map[profile_type][loc]);
		}
	    }
	}
    }
}

/* Process data in extend_auto_source_profile, save them into two maps.
   1. gimple_location to count.
   2. function_index to count.  */
void
extend_auto_profile::process_extend_source_profile ()
{
  struct cgraph_node *node;
  if (symtab->state == FINISHED)
    {
      return;
    }
  FOR_EACH_FUNCTION (node)
    {
      if (!gimple_has_body_p (node->decl) || node->inlined_to)
	{
	  continue;
	}

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	{
	  continue;
	}

      function *fn = DECL_STRUCT_FUNCTION (node->decl);
      push_cfun (fn);

      const function_instance *s
      = afdo_source_profile->get_function_instance_by_decl (
	  current_function_decl);

      if (s == NULL)
	{
	  pop_cfun ();
	  continue;
	}
      unsigned int decl_uid = DECL_UID (current_function_decl);
      gcov_type count = s->total_count ();
      if (dump_file)
	{
	  fprintf (dump_file, "Extend auto-profile for function %s.\n",
			       node->dump_name ());
	}
      event_func_map[profile_type][decl_uid] += count;
      set_loc_count ();
      pop_cfun ();
    }
  rank_all_func ();
}

/* Main entry of extend_auto_profile.  */

static void
extend_source_profile ()
{
  extend_profile = autofdo::extend_auto_profile::create ();
  if (dump_file)
    {
      if (extend_profile == NULL)
	{
	  fprintf (dump_file, "No profile file is found.\n");
	  return;
	}
      fprintf (dump_file, "Extend profile info generated.\n");
    }
}
} /* namespace autofdo.  */

/* Read the profile from the profile data file.  */

void
read_autofdo_file (void)
{
  if (auto_profile_file == NULL)
    auto_profile_file = DEFAULT_AUTO_PROFILE_FILE;

  autofdo::afdo_profile_info = XNEW (gcov_summary);
  autofdo::afdo_profile_info->runs = 1;
  autofdo::afdo_profile_info->sum_max = 0;

  /* Read the profile from the profile file.  */
  autofdo::read_profile ();
}

/* Free the resources.  */

void
end_auto_profile (void)
{
  delete autofdo::afdo_source_profile;
  delete autofdo::afdo_string_table;
  profile_info = NULL;
}

/* Extern function to get profile info in other passes.  */

bool
profile_exist (enum event_type type)
{
  return autofdo::extend_profile != NULL
	 && autofdo::extend_profile->auto_profile_exist (type);
}

gcov_type
event_get_loc_count (location_t loc, event_type type)
{
  return autofdo::extend_profile->get_loc_count (loc, type);
}

gcov_type
event_get_func_count (unsigned decl_uid, event_type type)
{
  return autofdo::extend_profile->get_func_count (decl_uid, type);
}

struct rank_info
event_get_func_rank (unsigned decl_uid, enum event_type type)
{
  return autofdo::extend_profile->get_func_rank (decl_uid, type);
}

gcov_type
event_get_topn_function_total_count_thres ()
{
  return autofdo::extend_profile->get_topn_function_total_count_thres ();
}

void
free_extend_profile_info ()
{
  if (autofdo::extend_profile != NULL)
    {
      delete autofdo::extend_profile;
    }
}

/* Returns TRUE if EDGE is hot enough to be inlined early.  */

bool
afdo_callsite_hot_enough_for_early_inline (struct cgraph_edge *edge)
{
  gcov_type count
      = autofdo::afdo_source_profile->get_callsite_total_count (edge);

  if (count > 0)
    {
      bool is_hot;
      profile_count pcount = profile_count::from_gcov_type (count).afdo ();
      gcov_summary *saved_profile_info = profile_info;
      /* At early inline stage, profile_info is not set yet. We need to
         temporarily set it to afdo_profile_info to calculate hotness.  */
      profile_info = autofdo::afdo_profile_info;
      is_hot = maybe_hot_count_p (NULL, pcount);
      profile_info = saved_profile_info;
      return is_hot;
    }

  return false;
}

namespace
{

const pass_data pass_data_ipa_auto_profile = {
  SIMPLE_IPA_PASS, "afdo", /* name */
  OPTGROUP_NONE,           /* optinfo_flags */
  TV_IPA_AUTOFDO,          /* tv_id */
  0,                       /* properties_required */
  0,                       /* properties_provided */
  0,                       /* properties_destroyed */
  0,                       /* todo_flags_start */
  0,                       /* todo_flags_finish */
};

class pass_ipa_auto_profile : public simple_ipa_opt_pass
{
public:
  pass_ipa_auto_profile (gcc::context *ctxt)
      : simple_ipa_opt_pass (pass_data_ipa_auto_profile, ctxt)
  {
  }

  /* opt_pass methods: */
  virtual bool
  gate (function *)
  {
    return flag_auto_profile;
  }
  virtual unsigned int
  execute (function *)
  {
    return autofdo::auto_profile ();
  }
}; // class pass_ipa_auto_profile

} // anon namespace

namespace
{
const pass_data pass_data_ipa_extend_auto_profile =
{
  SIMPLE_IPA_PASS, /* type */
  "ex-afdo", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_EXTEND_AUTO_PROFILE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_extend_auto_profile : public simple_ipa_opt_pass
{
public:
  pass_ipa_extend_auto_profile (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_extend_auto_profile, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) {return (flag_ipa_extend_auto_profile > 0);}
  virtual unsigned int execute (function *);

};

unsigned int
pass_ipa_extend_auto_profile::execute (function *fun)
{
  autofdo::extend_source_profile ();
  return 0;
}
} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_auto_profile (gcc::context *ctxt)
{
  return new pass_ipa_auto_profile (ctxt);
}

simple_ipa_opt_pass *
make_pass_ipa_extend_auto_profile (gcc::context *ctxt)
{
  return new pass_ipa_extend_auto_profile (ctxt);
}
