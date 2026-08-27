/* { dg-do compile } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-require-effective-target simdmath_vectorizes } */
/* { dg-options "-O3 -fopenmp -fno-openmp-simd -fsimdmath" } */

/* -fopenmp acts on omp declare simd by itself, so -fno-openmp-simd does
   not make the pre-included directives dormant and -fsimdmath is not
   inert here.  Saying otherwise was wrong twice: the vector call is
   emitted - below - and under -Werror the false report failed the build
   of any project whose CFLAGS carry -fopenmp, which is most of the ones
   this option was written for.

   No dg-warning: a diagnostic here is excess errors, which is the point.  */

#include <math.h>

double a[1024], b[1024];

void
f (void)
{
  int i;

  for (i = 0; i < 1024; i++)
    a[i] = pow (a[i], b[i]);
}

/* { dg-final { scan-assembler "_ZGVnN2vv_pow" } } */
