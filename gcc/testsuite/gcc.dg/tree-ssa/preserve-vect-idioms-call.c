/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Existing call restrictions take precedence over preservation.  */
extern void observe (void);

void __attribute__ ((noipa))
f (int *__restrict output, const signed char *__restrict weights,
   const int *__restrict biases, const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      for (unsigned int j = 0; j < 16; ++j)
	{
	  sum += weights[i * 32 + j] * input[j];
	  observe ();
	}
      output[i] = sum;
    }
}


/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "contains call and code would grow" "cunrolli" } } */
