/* { dg-do compile } */
/* { dg-require-effective-target simdmath_vectorizes } */
/* { dg-options "-O3 -fsimdmath -msimdmath-vec64" } */

/* -msimdmath-vec64 is the way back for a program that does have 64-bit
   variants of its own: it restores them for every one of the eleven
   names, which is also why it is not the default - it lets the compiler
   emit calls the packaged library cannot satisfy.
   simdmath-usersimd-5.c is the same source without it.  */

#pragma omp declare simd notinbranch
float sinf (float x) __attribute__ ((const));

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

/* { dg-final { scan-assembler "_ZGVnN2v_sinf" } } */
/* { dg-final { scan-assembler "_ZGVnN4v_sinf" } } */
