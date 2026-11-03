/* { dg-do run } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=16 -fdump-tree-linvsplit-details" } */

/* Exercise multi-level address definitions originally placed after the inner
   loop, two carries, a short final chunk, and zero/one outer iterations.  */
#define KERNEL(NAME, OPTIONS)                                            \
  __attribute__((noipa, optimize (OPTIONS)))                              \
  static void NAME (const unsigned char *restrict input,                  \
                    const float *restrict weights,                       \
                    const float *restrict bias, float *restrict output,  \
                    int n)                                               \
  {                                                                      \
    for (int i = 0; i < n; ++i)                                          \
      {                                                                  \
        float a = bias[i], b = bias[i] + 1;                               \
        for (int j = 0; j < 70; ++j)                                     \
          {                                                              \
            float v = (float) input[j] * weights[i * 70 + j];             \
            a += v;                                                      \
            b += v * 2;                                                  \
          }                                                              \
        output[3 * i + 1] = a;                                           \
        output[3 * i + 2] = b;                                           \
      }                                                                  \
  }

KERNEL (reference, "O0")
KERNEL (split, "O3")

int
main (void)
{
  unsigned char input[70];
  float weights[560], bias[8], expected[24], actual[24];
  int sizes[] = { 0, 1, 8 };
  for (int t = 0; t < 4; ++t)
    {
      for (int j = 0; j < 70; ++j)
	input[j] = (j + t) % 7 + 1;
      for (int i = 0; i < 560; ++i)
	weights[i] = ((i + t) % 13 - 6) * 0.125f;
      for (int i = 0; i < 8; ++i)
	bias[i] = i * 0.25f;
      for (int k = 0; k < 3; ++k)
	{
	  for (int i = 0; i < 24; ++i)
	    expected[i] = actual[i] = -100;
	  reference (input, weights, bias, expected, sizes[k]);
	  split (input, weights, bias, actual, sizes[k]);
	  for (int i = 0; i < 24; ++i)
	    if (expected[i] != actual[i])
	      __builtin_abort ();
	}
    }
  return 0;
}

/* { dg-final { scan-tree-dump "split inner loop into 5 chunks" "linvsplit" } } */
/* { dg-final { scan-tree-dump "chunk 4: .64, 70." "linvsplit" } } */
