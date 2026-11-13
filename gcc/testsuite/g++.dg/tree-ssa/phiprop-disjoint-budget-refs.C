/* Bound reference construction for a PHI with many incoming objects.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores --param=phiprop-disjoint-stores-max-work=4 -fchecking=2 -fdump-tree-phiprop1-details" } */

int refs (int a, int b, int c, int d, int e, int f, unsigned n, int *out)
{
  int *p;
  switch (n)
    {
    case 0: p = &a; break;
    case 1: p = &b; break;
    case 2: p = &c; break;
    case 3: p = &d; break;
    case 4: p = &e; break;
    default: p = &f; break;
    }
  *out = 1;
  return *p;
}

/* { dg-final { scan-tree-dump-not "PHI load propagation: crossed proven-disjoint local stores" "phiprop1" } } */
/* { dg-final { scan-tree-dump-times "PHI load propagation: disjoint-store work budget exhausted" 1 "phiprop1" } } */
