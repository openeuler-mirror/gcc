/* { dg-do compile } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores -fchecking=2 -fdump-tree-phiprop1-details" } */

#ifndef PHIPROP_ATTR
#define PHIPROP_ATTR
#endif

/* OUT cannot point into the current invocation's unescaped parameters.
   Unlike an unused local store, this store must survive until phiprop.  */
PHIPROP_ATTR int select_local (int x, int y, bool choose, int *out)
{
  int *p = choose ? &x : &y;
  *out = 1;
  return *p;
}
/* { dg-final { scan-tree-dump-times "PHI load propagation: crossed proven-disjoint local stores" 1 "phiprop1" } } */
