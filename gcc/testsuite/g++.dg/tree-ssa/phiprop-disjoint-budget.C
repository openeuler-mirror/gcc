/* The budget is shared across PHIs in one function, not reset per load.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores --param=phiprop-disjoint-stores-max-work=8 -fchecking=2 -fdump-tree-phiprop1-details" } */

int cumulative (int a, int b, int c, int d, bool first, bool second, int *out)
{
  int *p = first ? &a : &b;
  out[0] = 1;
  int x = *p;
  int *q = second ? &c : &d;
  out[1] = 2;
  return x ^ *q;
}

/* { dg-final { scan-tree-dump-times "PHI load propagation: crossed proven-disjoint local stores" 1 "phiprop1" } } */
/* { dg-final { scan-tree-dump-times "PHI load propagation: disjoint-store work budget exhausted" 1 "phiprop1" } } */
