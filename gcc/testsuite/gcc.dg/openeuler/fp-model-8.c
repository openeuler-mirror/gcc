/* { dg-do compile } */
/* { dg-options "-O2 -ffp-model=except -fcode-hoisting -fdump-tree-pre -ffat-lto-objects" } */
/* -ffat-lto-objects: the assertion below is a dump scan, and the dg
   layer adds that flag for scan-assembler* but not for
   scan-tree-dump*, so under an -flto board the dump lands in LTRANS
   and the test reports UNRESOLVED rather than either answer.  */

/* An explicit request on the command line wins over the model's
   defensive default: hoisting happens despite -ffp-model=except.

   The explicit option is written after the model here, so this test
   alone cannot distinguish explicitness-based arbitration from
   last-one-wins; fp-model-38.c is the order-reversed twin that can, and
   the two must be read as a pair.  */

double
g (int c, double x, double y)
{
  if (c)
    return x / y + 1.0;
  else
    return x / y - 1.0;
}

/* { dg-final { scan-tree-dump-times "x_\[0-9\]+\\(D\\) / y_\[0-9\]+\\(D\\)" 1 "pre" } } */
