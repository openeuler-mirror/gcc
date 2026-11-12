/* { dg-do compile } */
/* { dg-options "-O1 -fopenmp-simd -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=64 -fdump-tree-linvsplit-details" } */

/* At O1 the forced SIMD loop lets the pass process this function even
   though ordinary loop vectorization is disabled.  */
void
linvsplit (int *restrict out, const signed char *restrict weights,
	   const int *restrict bias, const unsigned char *restrict input)
{
  for (int i = 0; i < 16; ++i)
    {
      int sum = bias[i];
      int j = 0;
      do
        {
          sum += weights[i * 256 + j] * input[j];
          ++j;
        }
      while (j != 256);
      out[i] = sum;
    }
#pragma omp simd
  for (int k = 0; k < 256; ++k)
    out[16 + k] = input[k];
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 1 "linvsplit" } } */
/* { dg-final { scan-tree-dump-not "loop vectorization is disabled" "linvsplit" } } */
