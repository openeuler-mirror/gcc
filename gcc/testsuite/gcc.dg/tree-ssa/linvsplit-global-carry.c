/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=16 -fdump-tree-linvsplit-details" } */

/* A VAR_DECL in the carry reference does not make its address vary in the
   inner loop.  An overlapping global carry must still be rejected.  */
#define NI 8
#define NJ 64
unsigned char input[NJ];
int weights[NI * NJ], bias[NI], output[NI];

__attribute__ ((noipa)) void
global_carry (void)
{
  for (int i = 0; i < NI; ++i)
    {
      int sum = bias[i];
      for (int j = 0; j < NJ; ++j)
	sum += weights[i * NJ + j] * input[j];
      output[i] = sum;
    }
}

__attribute__ ((noipa)) void
overlapping_carry (void)
{
  for (int i = 0; i < NI; ++i)
    {
      int sum = bias[i];
      for (int j = 0; j < NJ; ++j)
	sum += weights[i * NJ + j] * input[j];
      output[i & 1] = sum;
    }
}

__attribute__ ((optimize ("O0"))) int
main (void)
{
  int expected[NI];
  for (int j = 0; j < NJ; ++j)
    input[j] = (j * 7 + 3) % 31;
  for (int i = 0; i < NI * NJ; ++i)
    weights[i] = (i * 11 + 5) % 29 - 14;
  for (int i = 0; i < NI; ++i)
    bias[i] = i * 3 - 7;

  for (int test = 0; test < 2; ++test)
    {
      for (int i = 0; i < NI; ++i)
	expected[i] = output[i] = -12345;
      for (int i = 0; i < NI; ++i)
	{
	  int sum = bias[i];
	  for (int j = 0; j < NJ; ++j)
	    sum += weights[i * NJ + j] * input[j];
	  expected[test ? i & 1 : i] = sum;
	}
      if (test)
	overlapping_carry ();
      else
	global_carry ();
      for (int i = 0; i < NI; ++i)
	if (output[i] != expected[i])
	  __builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 1 "linvsplit" } } */
/* { dg-final { scan-tree-dump "carry addresses may overlap across iterations" "linvsplit" } } */
