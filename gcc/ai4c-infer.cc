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

#include <unistd.h>
#include <math.h>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "ai4c-infer.h"
#include "config.h"
#include "system.h"

#define M_MODE_SIZE  6
#define NATIVE_TUNE_SIZE 128
#define CATS_STRINGS_ROW  12
#define CATS_STRINGS_COL  65
#define OFFSET_ROW  6
#define SCALE_ROW  6
#define UNITY_ROW 1
#define COEFFICIENT_ROW  18
#define COEFFICIENT_COL  100
#define COEFFICIENT1_ROW  100
#define COEFFICIENT1_COL  1
#define INTERCEPTS_ROW  100
#define INTERCEPTS1_ROW  1

/* Model info.  */
static int64_t argv_hw1[M_MODE_SIZE];
static char native_tune[NATIVE_TUNE_SIZE];

/* Intermediate computation results from the ONNX model.  */
static char cats_strings[CATS_STRINGS_ROW][CATS_STRINGS_COL];
static float offset[OFFSET_ROW];
static float scale[SCALE_ROW];
static float unity[UNITY_ROW];
static float coefficient[COEFFICIENT_ROW][COEFFICIENT_COL];
static float coefficient1[COEFFICIENT1_ROW][COEFFICIENT1_COL];
static float intercepts[INTERCEPTS_ROW];
static float intercepts1[INTERCEPTS1_ROW];

/* Model result.  */
static int64_t initialized;
static int64_t optimize_result;

void
prepare_native_tune_str (const char *info)
{
  if (info == NULL)
    {
      strcpy (native_tune, "=native+");
      return;
    }

  gcc_assert (strlen (info) < NATIVE_TUNE_SIZE);
  if (info)
    strcpy (native_tune, info);
  return;
}

void
set_cache_info (int prefetches, int l1_cache_size,
		int l1_cache_line_size, int l2_cache_size,
		int prefetch_latency, int prefetch_distance_factor)
{
  gcc_assert (5 < M_MODE_SIZE);
  argv_hw1[0] = prefetches;
  argv_hw1[1] = l1_cache_size;
  argv_hw1[2] = l1_cache_line_size;
  argv_hw1[3] = l2_cache_size;
  argv_hw1[4] = prefetch_latency;
  argv_hw1[5] = prefetch_distance_factor;
}

/* Read float from onnx.fdata.  */

float
read_float_from_file (FILE* file)
{
  char hex_float[8];
  float result;

  if (!file)
    {
      perror ("Can not open file.");
      return result;
    }
    
  if (fscanf (file, "%8s", hex_float) != 1)
    {
      perror ("Can not read hex from onnx.fdata.");
      return result;
    }

  unsigned char bytes[4];
  for (int i = 0; i < 4; i++)
    {
      sscanf(hex_float + 2 * i, "%2hhx", &bytes[i]);
    }

  memcpy(&result, bytes, sizeof(float));
  return result;
}

/* To read model parameter information from onnx.fdata and store it into the
   appropriate arrays.  */

static void
fill_node (const char *file_name)
{
  FILE *file = fopen (file_name, "rb");

  if (!file)
    {
      perror ("Can not open file.");
      return;
    }

   /* Read cats_strings from onnx.fdata.  */
  char hex_string[2];
  for (int i = 0; i < CATS_STRINGS_ROW; i++)
    {
      for (int j = 0; j < CATS_STRINGS_COL - 1; j++)
	{
	  if (fscanf(file, "%2s", hex_string) != 1)
	    {
	      perror ("Can not read cats_strings from onnx.fdata.");
	      return;
	    }
	  cats_strings[i][j] = (unsigned char)strtol(hex_string, NULL, 16);
	}
	cats_strings[i][CATS_STRINGS_COL - 1] = '\0';
    }
  
  /* Read offset from onnx.fdata.  */
  for (int i = 0; i < OFFSET_ROW; i++)
    {
      float result = read_float_from_file (file);
      offset[i] = result;
    }
  
  /* Read scale from onnx.fdata.  */
  for (int i = 0; i < SCALE_ROW; i++)
    {
      float result = read_float_from_file (file);
      scale[i] = result;
    }

  /* Read coefficient from onnx.fdata.  */
  for (int i = 0; i < COEFFICIENT_ROW; i++)
    for (int j = 0; j < COEFFICIENT_COL; j++)
      {
	float result = read_float_from_file (file);
	coefficient[i][j] = result;
      }

  /* Read coefficient1 from onnx.fdata.  */
  for (int i = 0; i < COEFFICIENT1_ROW; i++)
    for (int j = 0; j < COEFFICIENT1_COL; j++)
      {
	float result = read_float_from_file (file);
	coefficient1[i][j] = result;
      }

  /* Read intercepts from onnx.fdata.  */
  for (int i = 0; i < INTERCEPTS_ROW; i++)
    {
      float result = read_float_from_file (file);
      intercepts[i] = result;
    }

  /* Read intercepts1 from onnx.fdata.  */
  for (int i = 0; i < INTERCEPTS1_ROW; i++)
    {
      float result = read_float_from_file (file);
      intercepts1[i] = result;
    }

  /* Read unity from onnx.fdata.  */
  for (int i = 0; i < UNITY_ROW; i++)
    {
      float result = read_float_from_file (file);
      unity[i] = result;
    }

  fclose (file);
  return;
}

void
matmul (const float *lhs, const float *rhs, int m, int k, int n, float *out)
{
  for (int i = 0; i < m; i++)
    {
      for (int j = 0; j < n; j++)
	{
	  out[i * n + j] = 0.0f;
	  for (int p = 0; p < k; p++)
	    {
	      out[i * n + j] += lhs[i * k + p] * rhs[p * n + j];
	    }
	}
    }
}

void
add (const float *lhs, const float *rhs, int length, float *out)
{
  for (int i = 0; i < length; i++)
    {
      out[i] = lhs[i] + rhs[i];
    }
}

void
sub (const float *lhs, const float *rhs, int length, float *out)
{
  for (int i = 0; i < length; i++)
    {
      out[i] = lhs[i] - rhs[i];
    }
}

void
sigmoid (const float *in, int length, float *out)
{
  for (int i = 0; i < length; i++)
    {
      out[i] = 1.0f / (1.0f + expf (-in[i]));
    }
}

void
relu (const float *data, int length, float *out)
{
  for (int i = 0; i < length; i++)
    {
      if (data[i] < 0)
	{
	  out[i] = 0;
	}
      else
	{
	  out[i] = data[i];
	}
    }
}

void
line_concat (const float *in, int in_size, float *out, int out_size)
{
  for (int i = 0; i < in_size; i++)
    out[out_size + i] = in[i];
}

void
one_hot_encoder (const char *in, const char (*cats)[65], float *out,
		 int out_size)
{
  for (int i = 0; i < out_size; i++)
    {
      if (i < out_size && strcmp (cats[i], in) == 0)
	{
	  out[i] = 1.0f;
	}
      else
	{
	  out[i] = 0.0f;
	}
    }
}

void
imputer (const int64_t *in, int size, float *out)
{
  for (int i = 0; i < size; i++)
    out[i] = in[i] * 1.0f;
}

void
scaler (const float *in, const float *offset, const float *scale, int size,
	float *out)
{
  for (int i = 0; i < size; i++)
    out[i] = (in[i] - offset[i]) * scale[i];
}

int
argmax (const float *in, int in_size)
{
  int out_idx = 0;
  for (int i = 0; i < in_size; i++)
    {
      if (in[i] > in[out_idx])
	out_idx = i;
    }
  return out_idx;
}

static void
preprocess (int argc, int64_t *argv, int64_t *in_modes)
{
  int default_int_val= 0;
  for (int i = 0; i < argc && i < M_MODE_SIZE; i++)
    {
      if (i < argc)
	{
	  in_modes[i] = argv[i];
	}
      else
	{
	  in_modes[i] = default_int_val;
	}
    }
}

/* The process of model inference.  */
static int
graph_infer (int argc, const char *argv, int argc2, int64_t *argv2)
{
  const char *file_name = getenv ("GCC_AI4C_ONNX_FDATA");
  if (access (file_name, F_OK) == 0)
    fill_node (file_name);
  else
    return 0;

  int64_t in_modes[M_MODE_SIZE];

  preprocess (argc2, argv2, in_modes);
  
  /* concat_result and encoder_out are intermediate computation results from
     the ONNX model. concat_result is a 1 × 18 matrix, and encoder_out is a
     1 × 12 matrix.  */

  const int concat_out_size = 18;
  float concat_result[concat_out_size];
  const int encoder_out_size = 12;
  float encoder_out[encoder_out_size];

  one_hot_encoder (argv, cats_strings, encoder_out, encoder_out_size);

  line_concat (encoder_out, encoder_out_size, concat_result, 0);

  float variable[M_MODE_SIZE];
  imputer (in_modes, M_MODE_SIZE, variable);

  float variable1[M_MODE_SIZE];
  scaler (variable, offset, scale, M_MODE_SIZE, variable1);
  float transformed_column[concat_out_size + M_MODE_SIZE];
  line_concat (variable1, M_MODE_SIZE, transformed_column, 0);
  line_concat (concat_result, concat_out_size, transformed_column, 6);

  /* This requires performing matrix multiplication between a 1 × 18 matrix
     and an 18 × 100 matrix  */

  const int m = 1, k = 18, n = 100;
  float mul_result[n];
  matmul (transformed_column, coefficient[0], m, k, n, mul_result);

  float add_result[n];
  add (mul_result, intercepts, n, add_result);

  float next_activations[n];
  relu (add_result, n, next_activations);

  /* This requires performing matrix multiplication between a 1 × 100 matrix
     and an 100 × 1 matrix  */

  const int m2 = 1, k2 = 100, n2 = 1;
  float mul_result1[n2];
  matmul (next_activations, coefficient1[0], m2, k2, n2, mul_result1);

  float add_result1[n2];
  add (mul_result1, intercepts1, n2, add_result1);

  float out_activations_result[n2];
  sigmoid (add_result1, n2, out_activations_result);

  float negative_class_proba[n2];
  sub (unity, out_activations_result, n2, negative_class_proba);
  const int prob_size = n2 + n2;
  float probabilities[prob_size];
  line_concat (negative_class_proba, n2, probabilities, 0);
  line_concat (out_activations_result, n2, probabilities, n2);

  int argmax_output = argmax (probabilities, prob_size);
  return argmax_output;
}

void
execute_sha256 (const char *input, char *output, size_t output_size)
{
    char command[256];
    snprintf (command, sizeof (command), "echo -n \"%s\" | sha256sum", input);

    FILE *pipe = popen (command, "r");
    if (pipe == NULL)
      {
	perror ("Failed to run command.");
	return;
      }

    fgets (output, output_size, pipe);
    pclose (pipe);
}

int
get_optimize_decision_from_ai4c ()
{
  if (initialized== 1)
    return optimize_result;
  if (native_tune && (strchr (native_tune, '+') != NULL))
    {
      char hash[65];
      char input[64];
      const char prefix = '=';
      const char *start = strchr (native_tune, prefix);
      if (start)
	{
	  start += 1;
	  const char *end = strchr (start, '+');
	  if (!end)
	    {
	      end = native_tune + strlen (native_tune);
	    }
	  size_t len = end - start;
	  if (len >= sizeof (input))
	    len = sizeof (input) - 1;
	  strncpy (input, start, len);
	  input[len] = '\0';
	}
      else
	input[0] = '\0';

      execute_sha256 (input, hash, sizeof (hash));
      optimize_result = graph_infer (1, hash, M_MODE_SIZE, argv_hw1);
      initialized = 1;
      if (optimize_result == 1)
	setenv ("AI_GUIDED", "1", 1);
    }
  return optimize_result;
}
