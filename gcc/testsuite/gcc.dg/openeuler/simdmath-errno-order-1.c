/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-require-effective-target simdmath_vectorizes } */
/* { dg-options "-O3 -fno-fast-math -fsimdmath" } */

/* -fno-fast-math asks for errno only as one part of a bundle, so unlike
   an explicit -fmath-errno it does not survive a later -fsimdmath: the
   usual last-one-wins rule applies and the vector call is generated.
   simdmath-errno-order-2.c is the same pair the other way round, where
   errno is kept and the option reports itself inert.  The documentation
   used to claim neither order overrides what the user wrote, which is
   true of -fmath-errno and was never true of this.  */

#include <math.h>

double a[1024], b[1024];

void
f (void)
{
  int i;

  for (i = 0; i < 1024; i++)
    a[i] = exp (b[i]);
}

/* { dg-final { scan-assembler "_ZGVnN2v_exp" } } */
