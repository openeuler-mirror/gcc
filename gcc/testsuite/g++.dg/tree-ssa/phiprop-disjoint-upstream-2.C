/* { dg-do compile } */
/* { dg-options "-ftree-phiprop-disjoint-stores -O1 -fdump-tree-phiprop1-details -fdump-tree-release_ssa" } */

/* PR tree-optimization/116823 */
/* This lifetime-clobber case is handled by the separate upstream
   PR116823 extension.  The GCC 14 disjoint-store option does not implement
   that traversal.  Keep this undefined source compile-only and record the
   coverage boundary instead of claiming the upstream transformation.  */

static inline
const int &c(const int &d, const int &e) {
  if (d < e)
    return d;
  return e;
}

int g(int i, struct f *ff)
{
  const int &a = c(i, 10);
  return a;
}
/* { dg-final { scan-tree-dump-not "Inserting PHI for result of load" "phiprop1"} } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR" "release_ssa"} } */

