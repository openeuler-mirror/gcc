/* { dg-do compile } */
/* { dg-options "-O2 -fcode-hoisting -ffp-model=except -fdump-tree-pre -ffat-lto-objects" } */
/* -ffat-lto-objects: the assertion below is a dump scan, and the dg
   layer adds that flag for scan-assembler* but not for
   scan-tree-dump*, so under an -flto board the dump lands in LTRANS
   and the test reports UNRESOLVED rather than either answer.  */

/* fp-model-8.c with the order reversed: the explicit request comes
   BEFORE the model.  The arbitration is by explicitness, not position -
   the model fills only what the command line left unset - so hoisting
   must still happen.  fp-model-8.c alone cannot pin that: with the
   explicit option written after the model, a last-one-wins
   implementation passes it too.  This pair is what tells the two
   implementations apart.  */

double
g (int c, double x, double y)
{
  if (c)
    return x / y + 1.0;
  else
    return x / y - 1.0;
}

/* { dg-final { scan-tree-dump-times "x_\[0-9\]+\\(D\\) / y_\[0-9\]+\\(D\\)" 1 "pre" } } */
