/* { dg-do compile } */
/* { dg-options "-O2 -fsimdmath -fno-openmp-simd" } */

/* The same pair the other way round, which always worked.  The two files
   together say the rule is about what the user asked for and not about
   where they asked for it.  */

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
