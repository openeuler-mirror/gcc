/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Four body executions need only three latch executions in this loop.  */

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
      while (++j < 4);
      output[i] = sum;
    }
}

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "completely unrolled" "cunrolli" } } */
