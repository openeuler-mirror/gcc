/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a -fpreserve-vect-idioms -fdump-tree-cunrolli-details -fdump-tree-vect-details" } */

/* Retaining this loop can expose ordinary widening multiplication even
   without target dot-product support.  */

void __attribute__ ((noipa))
f (int *__restrict output, const short *__restrict weights,
   const int *__restrict biases, const short *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      for (unsigned int j = 0; j < 16; ++j)
	sum += weights[i * 32 + j] * input[j];
      output[i] = sum;
    }
}

/* { dg-final { scan-tree-dump "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" } } */
