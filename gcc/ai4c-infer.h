/* Lightweight AI Inference Framework.

   Copyright (C) 2024-2024 Free Software Foundation, Inc.

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

#ifndef AI4C_INFER_H
#define AI4C_INFER_H

extern int get_optimize_decision_from_ai4c ();
extern void set_cache_info (int prefetches, int l1_cache_size, 
			    int l1_cache_line_size, int l2_cache_size,
			    int prefetch_latency, int prefetch_distance_factor);
extern void prepare_native_tune_str (const char *info);
#endif /* AI4C_INFER_H */