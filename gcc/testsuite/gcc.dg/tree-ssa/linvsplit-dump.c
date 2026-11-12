/* { dg-do compile } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=64 -fdump-tree-linvsplit" } */

void
linvsplit (int *restrict out, const signed char *restrict weights,
	   const int *restrict bias, const unsigned char *restrict input)
{
  for (int i = 0; i < 16; ++i)
    {
      int sum = bias[i];
      for (int j = 0; j < 256; ++j)
	sum += weights[i * 256 + j] * input[j];
      out[i] = sum;
    }
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 1 "linvsplit" } } */
/* { dg-final { scan-tree-dump-not "optimized: loop invariant split" "linvsplit" } } */
