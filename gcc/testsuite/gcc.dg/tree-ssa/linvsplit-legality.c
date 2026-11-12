/* { dg-do run } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=16 -fdump-tree-linvsplit-details" } */

/* Each of these nests used to be split despite a missing legality proof.
   Keep the reference at O0 and require rejection, as well as correct output.  */
#define KERNEL(NAME, OPTIONS, INIT, STORE, UPDATE)                         \
  __attribute__((noipa, optimize (OPTIONS)))                              \
  static void NAME (const unsigned char *restrict input,                  \
                    const float *restrict weights,                       \
                    const float *restrict bias, float *restrict output,  \
                    int stride)                                          \
  {                                                                      \
    float state = 0;                                                     \
    for (int i = 0; i < 8; ++i)                                          \
      {                                                                  \
        float acc = (INIT), last = 0;                                    \
        for (int j = 0; j < 64; ++j)                                     \
          {                                                              \
            last = (float) input[j] * weights[i * 64 + j];                \
            acc += last;                                                 \
          }                                                              \
        STORE;                                                           \
        UPDATE;                                                          \
      }                                                                  \
  }

#define PAIR(NAME, INIT, STORE, UPDATE)                                   \
  KERNEL (NAME, "O3", INIT, STORE, UPDATE)                                \
  KERNEL (NAME##_ref, "O0", INIT, STORE, UPDATE)

PAIR (overlap, bias[i & 1], output[i & 1] = acc, (void) 0)
PAIR (dynamic_stride, bias[i * stride], output[i * stride] = acc, (void) 0)
PAIR (conditional, bias[i], if (last > 0) output[i] = acc, (void) 0)
PAIR (outer_state, bias[i] + state, output[i] = acc, state += last)

typedef void (*kernel) (const unsigned char *, const float *, const float *,
		       float *, int);

int
main (void)
{
  unsigned char input[64];
  float weights[512], bias[8], expected[8], actual[8];
  kernel tests[] = { overlap, dynamic_stride, conditional, outer_state };
  kernel refs[] = { overlap_ref, dynamic_stride_ref, conditional_ref,
		    outer_state_ref };
  for (int j = 0; j < 64; ++j)
    input[j] = j % 7 + 1;
  for (int i = 0; i < 512; ++i)
    weights[i] = (i % 13 - 6) * 0.125f;
  for (int i = 0; i < 8; ++i)
    bias[i] = i * 0.25f;

  for (int t = 0; t < 4; ++t)
    for (int stride = 0; stride < 2; ++stride)
      {
	for (int i = 0; i < 8; ++i)
	  expected[i] = actual[i] = 0;
	refs[t] (input, weights, bias, expected, stride);
	tests[t] (input, weights, bias, actual, stride);
	for (int i = 0; i < 8; ++i)
	  if (expected[i] != actual[i])
	    __builtin_abort ();
      }
  return 0;
}

/* { dg-final { scan-tree-dump "carry addresses may overlap across iterations" "linvsplit" } } */
/* { dg-final { scan-tree-dump "no carry store for inner live-out value" "linvsplit" } } */
/* { dg-final { scan-tree-dump "unsupported scalar or control dependence" "linvsplit" } } */
/* { dg-final { scan-tree-dump-not "split inner loop into" "linvsplit" } } */
