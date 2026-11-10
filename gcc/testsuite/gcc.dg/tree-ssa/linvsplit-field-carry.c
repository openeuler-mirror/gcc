/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=16 -fdump-tree-linvsplit-details" } */

/* A FIELD_DECL does not make a carry address vary in the inner loop.  The
   unrestricted variant must still be rejected when aliasing is unknown.  */
#define NI 8
#define NJ 64
unsigned char input[NJ];
int weights[NI * NJ], bias[NI];
struct result { int sum, guard; };

__attribute__ ((noipa)) void
field_carry (struct result *restrict output)
{
  for (int i = 0; i < NI; ++i)
    {
      int sum = bias[i];
      for (int j = 0; j < NJ; ++j)
	sum += weights[i * NJ + j] * input[j];
      output[i].sum = sum;
    }
}

__attribute__ ((noipa)) void
alias_unknown (struct result *output)
{
  for (int i = 0; i < NI; ++i)
    {
      int sum = bias[i];
      for (int j = 0; j < NJ; ++j)
	sum += weights[i * NJ + j] * input[j];
      output[i].sum = sum;
    }
}

__attribute__ ((optimize ("O0"))) int
main (void)
{
  int expected[NI];
  struct result output[NI];
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
  for (int test = 0; test < 2; ++test)
    {
      for (int i = 0; i < NI; ++i)
	output[i].sum = output[i].guard = -12345;
      if (test)
	alias_unknown (output);
      else
	field_carry (output);
      for (int i = 0; i < NI; ++i)
	if (output[i].sum != expected[i] || output[i].guard != -12345)
	  __builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 1 "linvsplit" } } */
/* { dg-final { scan-tree-dump "alias or dependence analysis is unknown" "linvsplit" } } */
