/* { dg-do compile } */
/* { dg-options "-O2 -floop-elim -fdump-tree-phiopt2-details" } */

/* The walk starts strictly past base and only moves forward, so "did it
   advance at all" is known without running the loop.  -floop-elim rewrites
   the distance computation to the constant 1, after which the loop has no
   remaining consumer.  */

int scan (int *base)
{
  int *p = base + 1;

  while (*p != 0)
    p++;

  return (unsigned int) (p - base) == 0;
}

/* { dg-final { scan-tree-dump "PHIOPT pattern optimization" "phiopt2" } } */
/* { dg-final { scan-tree-dump "= \\(unsigned int\\) 1;" "phiopt2" } } */
