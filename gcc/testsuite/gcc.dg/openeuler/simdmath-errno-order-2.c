/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-options "-O3 -fsimdmath -fno-fast-math" } */

/* The other order: -fno-fast-math comes last, errno is kept, and exp is
   left scalar - which the compiler now says rather than leaving it to be
   found.  */

#include <math.h>

double a[1024], b[1024];

void
f (void)
{
  int i;

  for (i = 0; i < 1024; i++)
    a[i] = exp (b[i]);
}

/* { dg-final { scan-assembler-not "_ZGVnN2v_exp" } } */
/* { dg-regexp {.*warning: '-fsimdmath' has no effect on the math functions that set 'errno' while '-fmath-errno' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: 'exp', 'expf', 'log', 'logf', 'pow', 'powf' and 'exp2f' are not vectorized; 'sin', 'sinf', 'cos' and 'cosf' still are} } */
