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

extern void matmul (const float *, const float *, int, int, int, float *);
extern void add (const float *, const float *, int, float *);
extern void sub (const float *, const float *, int, float *);
extern void sigmoid (const float *, int, float *);
extern void relu (const float *, int, float *);
extern void line_concat (const float *, int, float *, int);
extern void one_hot_encoder (const char *, const char (*)[65], float *, int);
extern void imputer (const int64_t *, int, float *);		 
extern void scaler (const float *, const float *, const float *, int, float *);
extern int argmax (const float *, int);

extern void
execute_sha256 (const char *, char *, size_t);
extern float read_float_from_file (FILE*);

extern int get_optimize_decision_from_ai4c ();
extern void get_optimize_decision_from_optimizer (int, const char **,
						 const char *, int ,
						 int64_t *);
extern void set_cache_info (int, int, int, int, int, int);
extern void prepare_native_tune_str (const char *);
#endif /* AI4C_INFER_H */
