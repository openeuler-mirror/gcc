/* { dg-do compile } */
/* { dg-require-effective-target simdmath_vectorizes } */
/* { dg-options "-O3 -fsimdmath" } */

/* -fsimdmath must not touch the user's own omp declare simd
   declarations.  The AArch64 hook drops the 64-bit clone for the
   functions the vector math library supplies, which has only 128-bit
   variants; the gate for that used to read "any declaration this
   translation unit does not define", which took this one in.  A program
   calling into its own vector library therefore lost the _ZGVnN2v_ call
   path the moment -fsimdmath appeared in CFLAGS, silently and with no
   way to ask for it back.

   Two element widths in one loop so that the narrower call really needs
   the 2-lane variant.  No simdmath_header requirement: nothing here
   comes from the pre-included header.  */

#pragma omp declare simd notinbranch
float myfun (float x) __attribute__ ((const));

void
loop (double *p, double *q, float *r, float *s, int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      p[i] = q[i] + 1.0;
      r[i] = myfun (s[i]);
    }
}

/* { dg-final { scan-assembler "_ZGVnN2v_myfun" } } */
