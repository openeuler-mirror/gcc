/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-options "-O3 -fsimdmath" } */
/* { dg-require-effective-target simdmath_vectorizes } */

/* The C and C++ half of the feature had no tests at all, which is how the
   -fopenmp-simd implication came to be untested: the declarations in
   simdmath.h are OpenMP pragmas, so without that implication nothing is
   ever cloned here and the option is silently inert.  The header pins
   simdlen, so unlike Fortran this path never asks for a 64-bit variant
   in the first place; the clone gate is exercised from simdmath-1.f90,
   where the shape can actually vary.  */

#include <math.h>

void
f (float *a, int n)
{
  int i;
  for (i = 0; i < n; i++)
    a[i] = sinf (a[i]);
}

/* { dg-final { scan-assembler {_ZGVnN4v_sinf} } } */
