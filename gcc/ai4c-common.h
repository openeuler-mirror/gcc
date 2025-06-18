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

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Implementation of model operators.  */

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

void
execute_sha256 (const char *input, char *output, size_t output_size)
{
    char command[256];
    snprintf (command, sizeof (command), "echo -n \"%s\" | sha256sum", input);

    FILE *pipe = popen (command, "r");
    if (pipe == NULL)
      {
	perror ("Failed to run sha256 command.");
	return;
      }

    fgets (output, output_size, pipe);
    pclose (pipe);
}

/* Read float from onnx.fdata.  */

float
read_float_from_file (FILE* file)
{
  char hex_float[8];
  float result;

  if (!file)
    {
      perror ("Can not open model file.");
      return result;
    }

  if (fscanf (file, "%8s", hex_float) != 1)
    {
      perror ("Can't read hex from model file.");
      return result;
    }

  unsigned char bytes[4];
  for (int i = 0; i < 4; i++)
    {
      sscanf (hex_float + 2 * i, "%2hhx", &bytes[i]);
    }

  memcpy (&result, bytes, sizeof (float));
  return result;
}