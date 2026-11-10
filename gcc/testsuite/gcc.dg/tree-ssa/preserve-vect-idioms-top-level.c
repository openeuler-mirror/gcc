/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Existing growth restrictions take precedence over preservation.  */
int __attribute__ ((noipa))
f (const signed char *__restrict weights,
   const unsigned char *__restrict input)
{
  int sum = 0;
  for (unsigned int j = 0; j < 16; ++j)
    sum += weights[j] * input[j];
  return sum;
}

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "size would grow" "cunrolli" } } */
