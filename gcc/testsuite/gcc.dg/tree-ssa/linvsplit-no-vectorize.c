/* { dg-do compile } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=64 -fno-tree-loop-vectorize -fdump-tree-linvsplit-details" } */

void
linvsplit (int *restrict out, const signed char *restrict weights,
	   const int *restrict bias, const unsigned char *restrict input)
{
  for (int i = 0; i < 16; ++i)
    {
      int sum = bias[i];
#pragma GCC ivdep
      for (int j = 0; j < 256; ++j)
	sum += weights[i * 256 + j] * input[j];
      out[i] = sum;
    }
}

/* { dg-final { scan-tree-dump "loop vectorization is disabled" "linvsplit" } } */
/* { dg-final { scan-tree-dump-not "split inner loop" "linvsplit" } } */
