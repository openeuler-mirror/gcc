/* { dg-do compile } */
/* { dg-options "-funsafe-math-optimizations -ffinite-math-only -ffp-model=except" } */

/* The driver cancels a whole -ffast-math for except as well as for the
   other two models, so the spellings -ffast-math is made of have to go
   the same way.  -ffinite-math-only is the sharpest case: it says NaN
   and Inf do not occur, which is the premise except exists to deny, and
   leaving it on let except fold __builtin_isnan to 0.  */

#if __FINITE_MATH_ONLY__ != 0
#error -ffinite-math-only survived -ffp-model=except
#endif

#ifdef __NO_SIGNED_ZEROS__
#error -ffp-model=except left signed zeros disabled
#endif

#ifdef __RECIPROCAL_MATH__
#error -ffp-model=except left reciprocal math enabled
#endif

#ifdef __NO_TRAPPING_MATH__
#error -ffp-model=except left trapping math disabled
#endif

#ifdef __NO_MATH_ERRNO__
#error -ffp-model=except left errno disabled
#endif

#ifndef __SUPPORT_SNAN__
#error -ffp-model=except did not ask for signaling NaNs
#endif

int
main (void)
{
  return 0;
}
