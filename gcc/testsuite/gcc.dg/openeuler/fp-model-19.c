/* { dg-do run } */
/* { dg-options "-O2 -funsafe-math-optimizations -ffinite-math-only -ffp-model=strict -ffat-lto-objects" } */
/* -ffat-lto-objects: the assertion below is a dump scan, and the dg
   layer adds that flag for scan-assembler* but not for
   scan-tree-dump*, so under an -flto board the dump lands in LTRANS
   and the test reports UNRESOLVED rather than either answer.  */

/* fp-model-17.c says the flags were cleared; this says the code
   generator agrees, without depending on how a dump spells anything.
   Under -ffinite-math-only __builtin_isnan folds to a constant 0 and the
   abort below is what a program gets instead of a NaN check.  */

volatile double zero = 0.0;

int
main (void)
{
  double nan = zero / zero;

  if (!__builtin_isnan (nan))
    __builtin_abort ();
  return 0;
}
