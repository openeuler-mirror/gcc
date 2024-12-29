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
#include <cassert>
#include "config.h"
#include "system.h"
#include "ai4c-infer.h"

#define M_OPTION_SIZE  11
#define M_MODE_SIZE  6
#define NATIVE_TUNE_SIZE  128
#define CATS_STRINGS_ROW  34
#define CATS_STRINGS_COL  65
#define CATS_STRINGS1_ROW  10
#define CATS_STRINGS1_COL  65
#define OFFSET_ROW  6
#define SCALE_ROW  6
#define UNITY_ROW  1
#define COEFFICIENT_ROW  356
#define COEFFICIENT_COL  10
#define COEFFICIENT1_ROW  10
#define COEFFICIENT1_COL  1
#define INTERCEPTS_ROW  10
#define INTERCEPTS1_ROW  1

/* Intermediate computation results from the ONNX model.  */
static char cats_strings[CATS_STRINGS_ROW][CATS_STRINGS_COL];
static char cats_strings1[CATS_STRINGS1_ROW][CATS_STRINGS1_COL];
static float offset[OFFSET_ROW];
static float scale[SCALE_ROW];
static float unity[UNITY_ROW];
static float coefficient[COEFFICIENT_ROW][COEFFICIENT_COL];
static float coefficient1[COEFFICIENT1_ROW][COEFFICIENT1_COL];
static float intercepts[INTERCEPTS_ROW];
static float intercepts1[INTERCEPTS1_ROW];

/* Return an integer that represents the comparison result of the
   two strings.  */

static int
compare_strings (const void *a, const void *b)
{
  const char *str_a = *(const char **)a;
  const char *str_b = *(const char **)b;

  int len = strlen (str_a) < strlen (str_b) ? strlen (str_a) : strlen (str_b);
  for (int i = 0; i < len; i++)
    {
      char c1 = str_a[i];
      char c2 = str_b[i];
      if (ISUPPER (c1) && !ISUPPER (c2))
	return 0;
      else if (!ISUPPER (c1) && ISUPPER (c2))
	return 1;
      else if (c1 != c2)
	return c1 < c2;
    }
  return strlen (str_a) > strlen (str_b);
}

/* Return the substring before the first underscore ('_') in the input
   string.  */

static void
truncate_prefix (const char *str, char *result)
{
  const char *underscore_pos = strchr (str, '_');
  if (underscore_pos == NULL)
    {
      strcpy (result, str);
      return;
    }

  size_t len = underscore_pos - str;
  strncpy (result, str, len + 1);
  result[len + 1] = '\0';
}


static void
preprocess (int argc1, const char **argv1, const char *mops,
	    int argc2, int64_t *argv2, char (*in_options)[1024],
	    int64_t *in_modes)
{
  strcpy (in_options[0], mops);

  const char *output_option = "-o";
  const char *marco_prefix = "-D";
  const char *needle = "--param";
  const char *flag_prefix = "-";
  const char *default_option = "-default-option";
  const int default_int_val = 0;
  int m_size = 0;
  for (int i = 0; i < argc1; i++)
    {
      if (strncmp (argv1[i], marco_prefix, 2) == 0)
	m_size ++;
    }

  char *m_options[m_size];
  char output_file[1024];
  int m_index = 0;
  for (int i = 0; i < argc1; i++)
    {
      if (strncmp (argv1[i], marco_prefix, 2) == 0)
	{
	  m_options[m_index] = (char *)argv1[i];
	  m_index ++;
	}
      if (strcmp (argv1[i], output_option) == 0)
	truncate_prefix (argv1[i + 1], output_file);
    }

  strcpy (in_options[1], output_file);
  int in_options_size = 2;
  qsort (m_options, m_size, sizeof (m_options[0]), compare_strings);
  for (int i = 0; i < m_size && in_options_size < M_OPTION_SIZE; i++)
    {
      strcpy (in_options[in_options_size], m_options[i]);
      in_options_size ++;
    }

  for (int i = 0; i < argc1 && in_options_size < M_OPTION_SIZE; i++)
    {
      if (strncmp (argv1[i], marco_prefix, 2) != 0
	  && strcmp (argv1[i], output_option) != 0
	  && strncmp (argv1[i], needle, 7) != 0
	  && strncmp (argv1[i], flag_prefix, 1) == 0)
	{
	  strcpy (in_options[in_options_size], argv1[i]);
	  in_options_size ++;
	}
    }

  while (in_options_size < M_OPTION_SIZE)
    {
      strcpy (in_options[in_options_size], default_option);
      in_options_size ++;
    }

  /* Use sha256 to encrypt the input.  */
  char hash[65];
  char input[64];
  for (int i = 0; i < M_OPTION_SIZE; i++)
    {
      execute_sha256 (in_options[i], hash, sizeof (hash));
      strcpy (in_options[i], hash);
    }

  for (int i = 0; i < argc2 && i < M_MODE_SIZE; i++)
    {
      if (i < argc2)
	in_modes[i] = argv2[i];
      else
	in_modes[i] = default_int_val;
    }
}

/* To read model parameter information from optimizer.fdata and store it into
   the appropriate arrays.  */

static void
fill_node (const char *file_name)
{
  FILE *file = fopen (file_name, "rb");

  if (!file)
    {
      perror ("Can not open file.");
      return;
    }

   /* Read cats_strings from optimizer.fdata.  */
  char hex_string[2];
  for (int i = 0; i < CATS_STRINGS_ROW; i++)
    {
      for (int j = 0; j < CATS_STRINGS_COL - 1; j++)
	{
	  if (fscanf (file, "%2s", hex_string) != 1)
	    {
	      perror ("Can not read cats_strings from optimizer.fdata.");
	      return;
	    }
	  cats_strings[i][j] = (unsigned char) strtol(hex_string, NULL, 16);
	}
      cats_strings[i][CATS_STRINGS_COL - 1] = '\0';
    }

  /* Read cats_strings1 from optimizer.fdata.  */
  for (int i = 0; i < CATS_STRINGS1_ROW; i++)
    {
      for (int j = 0; j < CATS_STRINGS1_COL - 1; j++)
	{
	  if (fscanf (file, "%2s", hex_string) != 1)
	    {
	      perror ("Can not read cats_strings1 from optimizer.fdata.");
	      return;
	    }
	  cats_strings1[i][j] = (unsigned char) strtol(hex_string, NULL, 16);
	}
      cats_strings1[i][CATS_STRINGS1_COL - 1] = '\0';
    }

  /* Read offset from optimizer.fdata.  */
  for (int i = 0; i < OFFSET_ROW; i++)
    {
      float result = read_float_from_file (file);
      offset[i] = result;
    }

  
  /* Read scale from optimizer.fdata.  */
  for (int i = 0; i < SCALE_ROW; i++)
    {
      float result = read_float_from_file (file);
      scale[i] = result;
    }

  /* Read unity from optimizer.fdata.  */
  for (int i = 0; i < UNITY_ROW; i++)
    {
      float result = read_float_from_file (file);
      unity[i] = result;
    }

  /* Read coefficient from optimizer.fdata.  */
  for (int i = 0; i < COEFFICIENT_ROW; i++)
    for (int j = 0; j < COEFFICIENT_COL; j++)
      {
	float result = read_float_from_file (file);
	coefficient[i][j] = result;
      }

  /* Read coefficient1 from optimizer.fdata.  */
  for (int i = 0; i < COEFFICIENT1_ROW; i++)
    for (int j = 0; j < COEFFICIENT1_COL; j++)
      {
	float result = read_float_from_file (file);
	coefficient1[i][j] = result;
      }

  /* Read intercepts from optimizer.fdata.  */
  for (int i = 0; i < INTERCEPTS_ROW; i++)
    {
      float result = read_float_from_file (file);
      intercepts[i] = result;
    }

  /* Read intercepts1 from optimizer.fdata.  */
  for (int i = 0; i < INTERCEPTS1_ROW; i++)
    {
      float result = read_float_from_file (file);
      intercepts1[i] = result;
    }

  fclose (file);
  return;
}

/* The process of model inference.  */

static int
graph_infer (int argc1, const char **argv1, const char *mops,
             int argc2, int64_t *argv2)
{
  char gcc_exec_prefix[512];
  ssize_t len = readlink ("/proc/self/exe", gcc_exec_prefix,
  			  sizeof (gcc_exec_prefix) - 1);
  if (len == -1)
    return 0;

  char native_file[512];
  strncpy (native_file, gcc_exec_prefix, sizeof (native_file) - 1);
  const char *target = "bin/gcc";
  const char *target_cc1 = "cc1";
  const char *target_gpp = "bin/g++";
  const char *target_cc1plus = "cc1plus";
  const char *target_gfortran = "bin/gfortran";
  const char *target_f951 = "f951";
  const char *replacement = "../libexec/gcc/optimizer.fdata";
  const char *replacement_front_end = "../../optimizer.fdata";

  /* Replace the part of the current executable file path after the last slash
     to locate the model file.  */
  if (strstr (native_file, target) != NULL ||
      strstr (native_file, target_gpp) != NULL ||
      strstr (native_file, target_gfortran) != NULL)
    {
      char *last_slash = strrchr (native_file, '/');
      if (last_slash != NULL)
	{
	  size_t prefix_len = last_slash - native_file + 1;
	  native_file[prefix_len] = '\0';
	  strncat (native_file, replacement, sizeof (native_file) -
		   strlen (native_file) - 1);
	}
    }
  else if (strstr (native_file, target_cc1) != NULL ||
	   strstr (native_file, target_cc1plus) != NULL ||
	   strstr (native_file, target_f951) != NULL)
    {
      char *last_slash = strrchr (native_file, '/');
      if (last_slash != NULL)
	{
	  size_t prefix_len = last_slash - native_file + 1;
	  native_file[prefix_len] = '\0';
	  strncat (native_file, replacement_front_end, sizeof (native_file) -
		   strlen (native_file) - 1);
	}
    }

  if (access (native_file, F_OK) == 0)
    fill_node (native_file);
  else
    return 0;

  static int64_t in_modes[M_MODE_SIZE];
  static char in_options[M_OPTION_SIZE][1024];

  preprocess (argc1, argv1, mops, argc2, argv2, in_options, in_modes);

  /* concat_result and encoder_out are intermediate computation results from
     the ONNX model. concat_result is a 1 × 18 matrix, and encoder_out is a
     1 × 12 matrix.  */

  const int concat_out_size = 350;
  float concat_result[concat_out_size];
  const int encoder_out_size = 34;
  const int encoder_last_size = 10;
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
  /* line_concat is used to stro*/
  line_concat (variable1, M_MODE_SIZE, transformed_column, 0);
  line_concat (concat_result, concat_out_size, transformed_column, 6);

  /* This requires performing matrix multiplication between a 1 × 356 matrix
     and an 356 × 10 matrix  */

  const int m = 1, k = 356, n = 10;
  float mul_result[n];
  matmul (transformed_column, coefficient[0], m, k, n, mul_result);  

  float add_result[n];
  add (mul_result, intercepts, n, add_result);

  float next_activations[n];
  relu (add_result, n, next_activations);
  
  /* This requires performing matrix multiplication between a 1 × 10 matrix
     and an 10 × 1 matrix  */

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
get_optimize_decision_from_optimizer (int argc, const char **argv,
				      const char *mops, int argc2,
				      int64_t *argv2)
{
  int model_pred = graph_infer (argc, argv, mops, argc2, argv2);
  if (model_pred == 1)
    {
      putenv ("AI_INFER_LEVEL=1");
    }
  return model_pred;
}
