/* { dg-do compile } */
/* { dg-options "-O2 -fsimdmath -fno-openmp-simd" } */

/* -fno-openmp-simd leaves the pre-included directives dormant, so the
   option does nothing at all - the third way of being inert, and the one
   that stayed silent after the other two were given a diagnostic.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp {.*warning: '-fsimdmath' has no effect while '-fno-openmp-simd' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: the declarations it pre-includes are 'omp declare simd' directives, which that option leaves dormant} } */
