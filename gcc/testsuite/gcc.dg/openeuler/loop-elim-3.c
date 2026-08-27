/* { dg-do compile } */
/* { dg-options "-O2 -floop-elim -fdump-tree-phiopt2-details" } */

/* Negative case: the walk starts at base itself, so a zero distance is a
   real possibility (the first element may already be zero) and the loop
   must be kept.  */

int scan_from_base (int *base)
{
  int *p = base;

  while (*p != 0)
    p++;

  return (unsigned int) (p - base) == 0;
}

/* { dg-final { scan-tree-dump-not "PHIOPT pattern optimization" "phiopt2" } } */
