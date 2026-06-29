/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+i8mm -fpreserve-vect-idioms" } */

void __attribute__ ((noipa))
f (int *__restrict output, const signed char *__restrict weights,
   const int *__restrict biases, const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      for (unsigned int j = 0; j < 16; ++j)
	sum += weights[i * 32 + j] * input[j];
      output[i] = sum;
    }
}

/* { dg-final { scan-assembler-times {\tusdot\t} 1 } } */
