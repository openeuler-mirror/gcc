/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt2-details" } */

/* Control: -floop-elim is off by default, so the same shape must keep its
   distance computation.  */

int scan (int *base)
{
  int *p = base + 1;

  while (*p != 0)
    p++;

  return (unsigned int) (p - base) == 0;
}

/* { dg-final { scan-tree-dump-not "PHIOPT pattern optimization" "phiopt2" } } */
