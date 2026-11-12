/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=64 -fdump-tree-linvsplit-details" } */

/* Exercise pointer carries and non-unit outer strides, without restrict,
   and an inner branch with an affine strided carry.  */
#define NI 16
#define NJ 256
unsigned char input[NJ];
signed char weights[NI * NJ];
int bias[NI], out[2 * NI], expected[2 * NI];

__attribute__ ((noipa)) void
pointer_carry (void)
{
  int *o = out;
  for (int i = 0; i < NI; i += 2)
    {
      int sum = bias[i];
      for (int j = 0; j < NJ; ++j)
	sum += weights[i * NJ + j] * input[j];
      *o = sum;
      o += 2;
    }
}

__attribute__ ((noipa)) void
inner_branch (int *restrict result)
{
  for (int i = 0; i < NI; ++i)
    {
      int sum = bias[i];
      for (int j = 0; j < NJ; ++j)
	if (input[j] & 1)
	  sum += weights[i * NJ + j] * input[j];
	else
	  sum -= weights[i * NJ + j] * input[j];
      result[2 * i + 1] = sum;
    }
}

__attribute__ ((optimize ("O0"))) int
main (void)
{
  for (int j = 0; j < NJ; ++j)
    input[j] = (j * 7 + 3) % 31;
  for (int i = 0; i < NI * NJ; ++i)
    weights[i] = (i * 11 + 5) % 29 - 14;
  for (int i = 0; i < NI; ++i)
    bias[i] = i * 3 - 7;

  for (int test = 0; test < 2; ++test)
    {
      for (int i = 0; i < 2 * NI; ++i)
	expected[i] = out[i] = -12345;
      for (int i = 0; i < NI; i += test ? 1 : 2)
	{
	  int sum = bias[i];
	  for (int j = 0; j < NJ; ++j)
	    if (!test || (input[j] & 1))
	      sum += weights[i * NJ + j] * input[j];
	    else
	      sum -= weights[i * NJ + j] * input[j];
	  expected[test ? 2 * i + 1 : i] = sum;
	}
      if (test)
	inner_branch (out);
      else
	pointer_carry ();
      for (int i = 0; i < 2 * NI; ++i)
	if (out[i] != expected[i])
	  __builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 2 "linvsplit" } } */
