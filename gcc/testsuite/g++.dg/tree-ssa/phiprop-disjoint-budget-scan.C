/* Statements without VDEFs must also consume the cumulative budget.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores --param=phiprop-disjoint-stores-max-work=8 -fchecking=2 -fdump-tree-phiprop1-details" } */

unsigned scan (unsigned a, unsigned b, bool choose, unsigned x, unsigned *out)
{
  unsigned *p = choose ? &a : &b;
  *out = 1;
  x ^= x << 13;
  x ^= x >> 17;
  x ^= x << 5;
  x ^= x << 13;
  x ^= x >> 17;
  x ^= x << 5;
  return x ^ *p;
}

/* { dg-final { scan-tree-dump-not "PHI load propagation: crossed proven-disjoint local stores" "phiprop1" } } */
/* { dg-final { scan-tree-dump-times "PHI load propagation: disjoint-store work budget exhausted" 1 "phiprop1" } } */
