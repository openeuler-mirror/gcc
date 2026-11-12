/* { dg-do compile } */
/* { dg-require-effective-target simdmath_vectorizes } */
/* { dg-options "-O3 -fsimdmath" } */

/* The accepted cost of withholding the 64-bit variants by name: a
   program that supplies its own vector sinf and declares it with omp
   declare simd loses the _ZGVnN2v_ call path while -fsimdmath is in
   force.  That is what the option means - the vector math comes from the
   packaged library, which has no 64-bit variants - and invoke.texi says
   so, but it was an undocumented surprise for two rounds and is pinned
   here so that it stays a decision rather than an accident.

   simdmath-usersimd-1.c is the other side: any name outside the eleven
   keeps its full clone set.  simdmath-vec64-1.c is the way back.  */

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

/* { dg-final { scan-assembler-not "_ZGVnN2v_sinf" } } */
/* { dg-final { scan-assembler "_ZGVnN4v_sinf" } } */
