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

#include "ai4c-common.h"

/* Input info.  */
#define M_OPTION_SIZE  11
#define M_MODE_SIZE  6

#define NATIVE_TUNE_SIZE 128

/* Dimension of model operator parameters.  */
#define SHA256SUM_OUTPUT_LENGTH 65
#define CATS_STRINGS_ROW  35
#define CATS_STRINGS1_ROW  1000
#define OFFSET_ROW  6
#define SCALE_ROW  6
#define UNITY_ROW  1
#define COEFFICIENT_ROW  1356
#define COEFFICIENT_COL  10
#define COEFFICIENT1_ROW  10
#define COEFFICIENT1_COL  1
#define INTERCEPTS_ROW  10
#define INTERCEPTS1_ROW  1

/* Intermediate results of the model, read in a fixed order.  */
static char cats_strings[CATS_STRINGS_ROW][SHA256SUM_OUTPUT_LENGTH];
static char cats_strings1[CATS_STRINGS1_ROW][SHA256SUM_OUTPUT_LENGTH];
static float offset[OFFSET_ROW];
static float scale[SCALE_ROW];
static float unity[UNITY_ROW];
static float coefficient[COEFFICIENT_ROW][COEFFICIENT_COL];
static float coefficient1[COEFFICIENT1_ROW][COEFFICIENT1_COL];
static float intercepts[INTERCEPTS_ROW];
static float intercepts1[INTERCEPTS1_ROW];

/* Model info.  */
static int64_t argv_hw1[M_MODE_SIZE];
char native_tune[NATIVE_TUNE_SIZE];

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

/* Read model parameter and store it into the appropriate arrays.  */

static void
fill_node (const char *file_name)
{
  FILE *file = fopen (file_name, "rb");

  if (!file)
    {
      perror ("Can not open file.");
      return;
    }

  char hex_string[2];
  for (int i = 0; i < CATS_STRINGS_ROW; i++)
    {
      for (int j = 0; j < SHA256SUM_OUTPUT_LENGTH - 1; j++)
	{
	  if (fscanf (file, "%2s", hex_string) != 1)
	    {
	      perror ("Can not read cats_strings from onnx.fdata.");
	      return;
	    }
	  cats_strings[i][j] = (unsigned char) strtol (hex_string, NULL, 16);
	}
      cats_strings[i][SHA256SUM_OUTPUT_LENGTH - 1] = '\0';
    }

  for (int i = 0; i < CATS_STRINGS1_ROW; i++)
    {
      for (int j = 0; j < SHA256SUM_OUTPUT_LENGTH - 1; j++)
	{
	  if (fscanf (file, "%2s", hex_string) != 1)
	    {
	      perror ("Can not read cats_strings1 from onnx.fdata.");
	      return;
	    }
	  cats_strings1[i][j] = (unsigned char) strtol (hex_string, NULL, 16);
	}
      cats_strings1[i][SHA256SUM_OUTPUT_LENGTH - 1] = '\0';
    }

  for (int i = 0; i < OFFSET_ROW; i++)
    {
      float result = read_float_from_file (file);
      offset[i] = result;
    }

  for (int i = 0; i < SCALE_ROW; i++)
    {
      float result = read_float_from_file (file);
      scale[i] = result;
    }

  for (int i = 0; i < UNITY_ROW; i++)
    {
      float result = read_float_from_file (file);
      unity[i] = result;
    }

  for (int i = 0; i < COEFFICIENT_ROW; i++)
    for (int j = 0; j < COEFFICIENT_COL; j++)
      {
	float result = read_float_from_file (file);
	coefficient[i][j] = result;
      }

  for (int i = 0; i < COEFFICIENT1_ROW; i++)
    for (int j = 0; j < COEFFICIENT1_COL; j++)
      {
	float result = read_float_from_file (file);
	coefficient1[i][j] = result;
      }

  for (int i = 0; i < INTERCEPTS_ROW; i++)
    {
      float result = read_float_from_file (file);
      intercepts[i] = result;
    }

  for (int i = 0; i < INTERCEPTS1_ROW; i++)
    {
      float result = read_float_from_file (file);
      intercepts1[i] = result;
    }

  fclose (file);
  return;
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

  static int64_t in_modes[M_MODE_SIZE];
  static char in_options[M_OPTION_SIZE][1024];
  strcpy (in_options[0], argv);

  const int concat_out_size = COEFFICIENT_ROW;
  float concat_result[concat_out_size];
  const int encoder_out_size = CATS_STRINGS_ROW;
  const int encoder_last_size = CATS_STRINGS1_ROW;
  int concat_size = 0;
  const int size = encoder_out_size;

  for (int i = 1; i < M_OPTION_SIZE; i++)
    {
      float encoder_out[size];
      one_hot_encoder (in_options[i], cats_strings, encoder_out, size);
      line_concat (encoder_out, size, concat_result, concat_size);
      concat_size += size;
    }

    float encoder_out2[encoder_last_size];
    one_hot_encoder (in_options[0], cats_strings1, encoder_out2,
      encoder_last_size);
    line_concat (encoder_out2, encoder_last_size, concat_result, concat_size);
    concat_size += encoder_last_size;

    float variable[M_MODE_SIZE];
    imputer (in_modes, M_MODE_SIZE, variable);
    float variable1[M_MODE_SIZE];
    scaler (variable, offset, scale, M_MODE_SIZE, variable1);

    float transformed_column[concat_out_size + M_MODE_SIZE];
    line_concat (variable1, M_MODE_SIZE, transformed_column, 0);
    line_concat (concat_result, concat_out_size, transformed_column, 6);

    const int m = 1, k = COEFFICIENT_ROW, n = COEFFICIENT_COL;
    float mul_result[n];
    matmul (transformed_column, coefficient[0], m, k, n, mul_result);

    float add_result[n];
    add (mul_result, intercepts, n, add_result);

    float next_activations[n];
    relu (add_result, n, next_activations);

    const int m2 = 1, k2 = 10, n2 = 1;
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
