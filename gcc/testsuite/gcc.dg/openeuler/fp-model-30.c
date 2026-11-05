/* { dg-do compile } */
/* { dg-options "-O2 -funsafe-math-optimizations -ffinite-math-only -ffp-model=precise" } */

/* The precise counterpart of fp-model-17.c.  precise is the model the
   documentation recommends first and the one a user reaching for
   reproducible results is most likely to pick, and it had no test at all
   for either half of what it does: deleting its set_fast_math_flags call
   or its -frounding-math left the whole suite green.  fp-model-17/18
   cover strict and except only, and simdmath-inert-* has three strict,
   one except, one fast and no precise.  */

#if __FINITE_MATH_ONLY__ != 0
#error -ffinite-math-only survived -ffp-model=precise
#endif

#ifdef __NO_SIGNED_ZEROS__
#error -ffp-model=precise left signed zeros disabled
#endif

#ifdef __RECIPROCAL_MATH__
#error -ffp-model=precise left reciprocal math enabled
#endif

#ifdef __ASSOCIATIVE_MATH__
#error -ffp-model=precise left associative math enabled
#endif

#ifndef __SUPPORT_SNAN__
#error -ffp-model=precise did not ask for signaling NaNs
#endif

/* Contraction off is the other half of what precise does, and no
   predefined macro reports it - __FP_FAST_FMA describes the hardware,
   not the setting, and is defined either way.  Assert it on the code:
   with contraction on this is one fmadd.  */

double
contract (double a, double b, double c)
{
  return a * b + c;
}

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler "fmul" } } */
/* { dg-final { scan-assembler "fadd" } } */
/* { dg-final { scan-assembler-not "fmadd" } } */
