/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+dotprod -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

void __attribute__ ((noipa))
f (unsigned int *__restrict output, const unsigned char *__restrict weights,
   const unsigned int *__restrict biases, const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      unsigned int sum = biases[i];
      for (unsigned int j = 0; j < 16; ++j)
	sum += weights[i * 32 + j] * input[j];
      output[i] = sum;
    }
}

/* { dg-final { scan-tree-dump "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-assembler-times {\tudot\t} 1 } } */
