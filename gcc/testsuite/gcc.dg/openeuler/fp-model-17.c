/* { dg-do compile } */
/* { dg-options "-funsafe-math-optimizations -ffinite-math-only -ffp-model=strict" } */

/* strict is documented as precise plus except, so it must be at least as
   strong as precise.  It used to be the weakest of the three: only
   precise cancelled the fast-math settings, so -ffinite-math-only
   survived strict while it did not survive precise.  The command line
   that matters is this one - a project whose CFLAGS already ask for fast
   math, appending a model to get IEEE behavior back.  */

#if __FINITE_MATH_ONLY__ != 0
#error -ffinite-math-only survived -ffp-model=strict
#endif

#ifdef __NO_SIGNED_ZEROS__
#error -ffp-model=strict left signed zeros disabled
#endif

#ifdef __RECIPROCAL_MATH__
#error -ffp-model=strict left reciprocal math enabled
#endif

#ifdef __ASSOCIATIVE_MATH__
#error -ffp-model=strict left associative math enabled
#endif

#ifdef __NO_TRAPPING_MATH__
#error -ffp-model=strict left trapping math disabled
#endif

#ifdef __NO_MATH_ERRNO__
#error -ffp-model=strict left errno disabled
#endif

#ifndef __SUPPORT_SNAN__
#error -ffp-model=strict did not ask for signaling NaNs
#endif

int
main (void)
{
  return 0;
}
