/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=64 -fdump-tree-linvsplit-details" } */

/* Unsigned IVs, a narrow inner IV with a short final chunk, and inclusive
   bounds must all preserve the complete iteration space.  */
#define NI 16
#define NJ 200

#define KERNEL(NAME, ITYPE, JTYPE, ITEST, JTEST)                           \
  __attribute__ ((noipa)) void                                          \
  NAME (int *restrict out, const signed char *restrict weights,          \
	const int *restrict bias, const unsigned char *restrict input)   \
  {                                                                    \
    for (ITYPE i = 0; ITEST; ++i)                                       \
      {                                                                \
	int sum = bias[i];                                              \
	for (JTYPE j = 0; JTEST; ++j)                                    \
	  sum += weights[i * NJ + j] * input[j];                         \
	out[i] = sum;                                                   \
      }                                                                \
  }

KERNEL (unsigned_iv, unsigned int, unsigned int, i < NI, j < NJ)
KERNEL (narrow_iv, int, unsigned char, i < NI, j < NJ)
KERNEL (inclusive_bound, int, int, i <= NI - 1, j <= NJ - 1)

typedef void (*kernel) (int *, const signed char *, const int *,
		       const unsigned char *);

__attribute__ ((optimize ("O0"))) int
main (void)
{
  unsigned char input[NJ];
  signed char weights[NI * NJ];
  int bias[NI], actual[NI], expected[NI];
  kernel tests[] = { unsigned_iv, narrow_iv, inclusive_bound };

  for (int j = 0; j < NJ; ++j)
    input[j] = (j * 7 + 3) % 31;
  for (int i = 0; i < NI * NJ; ++i)
    weights[i] = (i * 11 + 5) % 29 - 14;
  for (int i = 0; i < NI; ++i)
    {
      bias[i] = i * 3 - 7;
      int sum = bias[i];
      for (int j = 0; j < NJ; ++j)
	sum += weights[i * NJ + j] * input[j];
      expected[i] = sum;
    }

  for (unsigned k = 0; k < sizeof tests / sizeof tests[0]; ++k)
    {
      for (int i = 0; i < NI; ++i)
	actual[i] = -12345;
      tests[k] (actual, weights, bias, input);
      for (int i = 0; i < NI; ++i)
	if (actual[i] != expected[i])
	  __builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 3 "linvsplit" } } */
/* { dg-final { scan-tree-dump-times "chunk 3: .192, 200." 3 "linvsplit" } } */
