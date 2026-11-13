/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Do not look through truncation, or classify ordinary full-width
   multiplication as a widened multiplication.  */

void __attribute__ ((noipa))
truncated (int *__restrict output, const signed char *__restrict weights,
	   const int *__restrict biases, const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      for (unsigned int j = 0; j < 16; ++j)
	sum += (unsigned char) (weights[i * 32 + j] * input[j]);
      output[i] = sum;
    }
}

void __attribute__ ((noipa))
plain (int *__restrict output, const int *__restrict weights,
       const int *__restrict biases, const int *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      for (unsigned int j = 0; j < 16; ++j)
	sum += weights[i * 32 + j] * input[j];
      output[i] = sum;
    }
}

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
