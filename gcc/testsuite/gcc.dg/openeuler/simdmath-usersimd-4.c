/* { dg-do compile } */
/* { dg-require-effective-target simdmath_vectorizes } */
/* { dg-options "-std=gnu89 -fno-inline -O3 -fsimdmath" } */

/* A gnu89 extern inline definition: the body is visible to this unit,
   yet there is no external definition, so DECL_EXTERNAL stays set and
   the gate withholds the 64-bit clone.  That is the right answer -
   callers link against the library's sinf, not this unit's inline body.

   This construct was once believed to separate DECL_EXTERNAL from the
   negated cgraph definition flag; measured, the two agree here as they
   do in every other construction tried.  What this test pins is the
   behaviour on this construct, not the spelling of the gate.  */

#pragma omp declare simd notinbranch
extern __attribute__ ((gnu_inline)) inline float
sinf (float x)
{
  return x * x + 1.0f;
}

void
loop (double *p, double *q, float *r, float *s, int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      p[i] = q[i] + 1.0;
      r[i] = sinf (s[i]);
    }
}

/* { dg-final { scan-assembler-not "_ZGVnN2v_sinf" } } */
/* { dg-final { scan-assembler "_ZGVnN4v_sinf" } } */
