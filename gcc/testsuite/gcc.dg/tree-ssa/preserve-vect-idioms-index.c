/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* The conditional index recurrence is not a simple reduction.  */
void __attribute__ ((noipa))
f (int *__restrict output, unsigned int *__restrict indices,
   const signed char *__restrict weights, const int *__restrict biases,
   const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      int best = -129;
      unsigned int index = 0;
      for (unsigned int j = 0; j < 16; ++j)
	{
	  sum += weights[i * 32 + j] * input[j];
	  int w = weights[i * 32 + j];
	  if (w > best)
	    {
	      best = w;
	      index = j;
	    }
	}
      output[i] = sum;
      indices[i] = index;
    }
}

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "loop with 16 iterations completely unrolled" "cunrolli" } } */
