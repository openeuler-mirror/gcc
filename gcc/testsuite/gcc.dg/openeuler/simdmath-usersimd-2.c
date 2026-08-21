/* { dg-do compile } */
/* { dg-require-effective-target simdmath_vectorizes } */
/* { dg-options "-O3 -fsimdmath" } */

/* Two things the simd-clone gate must not take away, neither of which
   had a test.

   myfun is a built-in name - every libm name is - declared by the
   program with omp declare simd, which is how a project hands its own
   vector library to the vectorizer.  Narrowing the gate to built-ins was
   not enough on its own; it has to name the eleven functions the vector
   math library actually covers.

   defined_sinf is defined here, and node->definition keeps the full
   upstream clone set for anything this translation unit defines.  */

#pragma omp declare simd notinbranch
float hypotf (float x, float y) __attribute__ ((const));

#pragma omp declare simd notinbranch
float defined_sinf (float x);

float
defined_sinf (float x)
{
  return x * x;
}

void
loop (double *p, double *q, float *r, float *s, float *t, int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      p[i] = q[i] + 1.0;
      r[i] = hypotf (s[i], t[i]) + defined_sinf (t[i]);
    }
}

/* { dg-final { scan-assembler "_ZGVnN2vv_hypotf" } } */
/* { dg-final { scan-assembler "_ZGVnN2v_defined_sinf" } } */
