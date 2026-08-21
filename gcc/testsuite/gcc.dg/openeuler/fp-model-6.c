/* { dg-do compile } */
/* { dg-options "-O2 -ffp-model=precise -fdump-tree-pre -ffat-lto-objects" } */
/* -ffat-lto-objects: the assertion below is a dump scan, and the dg
   layer adds that flag for scan-assembler* but not for
   scan-tree-dump*, so under an -flto board the dump lands in LTRANS
   and the test reports UNRESOLVED rather than either answer.  */

/* -ffp-model=precise constrains floating-point semantics; it must not
   switch off general optimizations that have nothing to do with them.
   Code hoisting still merges the division computed on both arms into
   the dominating block, as at plain -O2.  */

double
g (int c, double x, double y)
{
  if (c)
    return x / y + 1.0;
  else
    return x / y - 1.0;
}

/* { dg-final { scan-tree-dump-times "x_\[0-9\]+\\(D\\) / y_\[0-9\]+\\(D\\)" 1 "pre" } } */
