/* { dg-do compile } */
/* { dg-options "-ftree-phiprop-disjoint-stores -O1 -fdump-tree-phiprop1-details -fdump-tree-release_ssa" } */

/* PR tree-optimization/116823 */
/* The clobber on b should not get in the way of phiprop here. */
/* We should have MIN_EXPR early on. */

void f(int *);

int g(int i)
{
  const int t = 10;
  const int *a;
  {
    int b;
    f(&b);
    if (t < i)
      a = &t;
    else
      a = &i;
  }
  return *a;
}

/* { dg-final { scan-tree-dump-times "Inserting PHI for result of load" 1 "phiprop1"} } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "release_ssa"} } */

