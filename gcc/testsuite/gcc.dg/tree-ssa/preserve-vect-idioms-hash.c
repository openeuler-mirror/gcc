/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* An additional non-affine scalar recurrence prevents vectorization.  */
void __attribute__ ((noipa))
f (int *__restrict output, unsigned int *__restrict hash,
   const signed char *__restrict weights, const int *__restrict biases,
   const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      unsigned int h = i;
      for (unsigned int j = 0; j < 16; ++j)
	{
	  sum += weights[i * 32 + j] * input[j];
	  h = h * 31 + (unsigned char) weights[i * 32 + j];
	}
      output[i] = sum;
      hash[i] = h;
    }
}

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "loop with 16 iterations completely unrolled" "cunrolli" } } */
