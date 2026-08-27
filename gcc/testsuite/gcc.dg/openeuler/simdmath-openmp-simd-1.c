/* { dg-do compile } */
/* { dg-options "-O2 -fno-openmp-simd -fsimdmath" } */

/* -fsimdmath turns -fopenmp-simd on, but not over the user's head.  This
   is the order that used to lose: the coupling was a bare assignment, so
   a -fno-openmp-simd written before -fsimdmath was overwritten, while the
   same pair the other way round was honored.  -fopenmp-simd is the far
   reaching half of the coupling - it also activates omp simd directives
   already in the source, like the one below, which is exactly what
   -fno-openmp-simd is written to prevent.  */

#pragma omp declare simd
double
f (double x)
{
  return x * 2.0;
}

/* { dg-final { scan-assembler-not "_ZGV" } } */

/* The option is inert here, and now says so; these two files are what
   pinned the ordering rule that makes it inert.  */
/* { dg-regexp {.*warning: '-fsimdmath' has no effect while '-fno-openmp-simd' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: the declarations it pre-includes are 'omp declare simd' directives, which that option leaves dormant} } */
