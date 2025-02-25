/* auto-profile.h - Defines data exported from auto-profile.cc
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

#ifndef AUTO_PROFILE_H
#define AUTO_PROFILE_H

enum event_type
{
  INST_EXEC = 0,
  CACHE_MISSES,
  PMU_EVENT,
  EVENT_NUMBER
};

/* Read, process, finalize AutoFDO data structures.  */
extern void read_autofdo_file (void);
extern void end_auto_profile (void);

/* Returns TRUE if EDGE is hot enough to be inlined early.  */
extern bool afdo_callsite_hot_enough_for_early_inline (struct cgraph_edge *);

/* Chcek if profile exists before using this profile.  */
extern bool profile_exist (enum event_type);

/* Given func decl_uid or gimple location and event_type, return count.
   Count is 0 if function or gimple is not sampled.  */
extern gcov_type event_get_func_count (unsigned, enum event_type);
extern gcov_type event_get_loc_count (location_t, enum event_type);
extern gcov_type event_get_topn_function_total_count_thres ();

struct rank_info
{
  unsigned total;
  unsigned rank;
};

/* Given function decl_uid and event type, return rank_info.  Rank_info
   is {0, 0} if function was not sampled.  */
extern struct rank_info event_get_func_rank (unsigned, enum event_type);

/* Free memory allocated by autofdo::extern_profile.  */
extern void free_extend_profile_info ();

#endif /* AUTO_PROFILE_H */
