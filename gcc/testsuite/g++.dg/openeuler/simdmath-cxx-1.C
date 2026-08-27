// { dg-do compile { target aarch64*-*-* } }
// { dg-require-effective-target simdmath_header }
// { dg-options "-O3 -fsimdmath" }
/* { dg-require-effective-target simdmath_vectorizes } */

// Same as the C case, through the C++ front end: the header wraps its
// declarations in extern "C" with glibc's exception specification, and
// the vectorizer has to reach them the same way.

#include <cmath>

void
f (float *a, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = sinf (a[i]);
}

// { dg-final { scan-assembler {_ZGVnN4v_sinf} } }
