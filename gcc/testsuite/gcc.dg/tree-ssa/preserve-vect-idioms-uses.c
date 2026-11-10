/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Neither the PHI result nor the updated sum may have another in-loop
   consumer.  These are prefix sums, not simple reductions.  */

void __attribute__ ((noipa))
prefix_before (int *__restrict output, const signed char *__restrict weights,
	       const int *__restrict biases,
	       const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      for (unsigned int j = 0; j < 16; ++j)
	{
	  output[i * 16 + j] = sum;
	  sum += weights[i * 32 + j] * input[j];
	}
    }
}

void __attribute__ ((noipa))
prefix_after (int *__restrict output, const signed char *__restrict weights,
	      const int *__restrict biases,
	      const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      for (unsigned int j = 0; j < 16; ++j)
	{
	  sum += weights[i * 32 + j] * input[j];
	  output[i * 16 + j] = sum;
	}
    }
}

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
