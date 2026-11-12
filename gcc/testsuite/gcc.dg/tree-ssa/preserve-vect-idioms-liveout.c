/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* A latch-tested loop can use the updated sum outside the loop.  Multiple
   consumers of the final result must not be mistaken for prefix sums.  */

void __attribute__ ((noipa))
f (int *__restrict output, const signed char *__restrict weights,
   const int *__restrict biases, const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      unsigned int j = 0;
      do
	{
	  sum += weights[i * 32 + j] * input[j];
	}
      while (++j < 16);
      output[i * 2] = sum;
      output[i * 2 + 1] = sum ^ 1;
    }
}

/* { dg-final { scan-tree-dump "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump-not "completely unrolled" "cunrolli" } } */
