/* { dg-do compile } */
/* { dg-options "-ffinite-math-only -ffp-model=strict -fno-fast-math" } */

/* Only the positive spelling is cancelled by the model.  A -fno-fast-math
   asks for the same thing the model does, and dropping it would leave the
   flags it was there to clear switched on - a stricter model producing
   less strict code.  __FINITE_MATH_ONLY__ tracks exactly that: it is 1
   with -ffinite-math-only alone and 0 once -fno-fast-math has cleared
   it.  */
#if __FINITE_MATH_ONLY__
#error -fno-fast-math was cancelled, so -ffinite-math-only survived
#endif

int
main (void)
{
  return 0;
}
