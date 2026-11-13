/* { dg-do compile } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=16 --param loop-invariant-split-max-copies=4 -fdump-tree-linvsplit-details" } */

/* Analyze a rejected nest once, and transform all five independent eligible
   nests.  The per-nest copy limit must not cap the number of transformations
   in the function.  Different outputs keep the nests from being eliminated.  */
#define NEST(K, VALUE)                                                   \
  for (int i = 0; i < 8; ++i)                                           \
    {                                                                   \
      float acc = bias[i];                                              \
      for (int j = 0; j < 64; ++j)                                      \
        acc += (VALUE) * weights[i * 64 + j];                            \
      output[(K) * 8 + i] = acc;                                        \
    }

void
worklist (const unsigned char *restrict input,
	  const float *restrict weights, const float *restrict bias,
	  float *restrict output)
{
  NEST (0, 1.0f)
  NEST (1, (float) input[j])
  NEST (2, (float) input[j] + 1)
  NEST (3, (float) input[j] + 2)
  NEST (4, (float) input[j] + 3)
  NEST (5, (float) input[j] + 4)
}

/* { dg-final { scan-tree-dump-times "no outer-invariant inner load" 1 "linvsplit" } } */
/* { dg-final { scan-tree-dump-times "optimized: loop invariant split: split inner loop into 4 chunks" 5 "linvsplit" } } */
