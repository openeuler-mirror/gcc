/* { dg-do compile } */
/* { dg-options "-O2 -ffp-model=except -fdump-tree-pre -ffat-lto-objects" } */
/* -ffat-lto-objects: the assertion below is a dump scan, and the dg
   layer adds that flag for scan-assembler* but not for
   scan-tree-dump*, so under an -flto board the dump lands in LTRANS
   and the test reports UNRESOLVED rather than either answer.  */

/* -ffp-model=except preserves exception semantics, so optimizations
   that move floating-point computations across control flow are turned
   off: the division stays on each arm instead of being hoisted into
   the dominating block.  */

double
g (int c, double x, double y)
{
  if (c)
    return x / y + 1.0;
  else
    return x / y - 1.0;
}

/* { dg-final { scan-tree-dump-times "x_\[0-9\]+\\(D\\) / y_\[0-9\]+\\(D\\)" 2 "pre" } } */
