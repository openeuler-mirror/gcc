/* { dg-do compile } */
/* { dg-options "-O2 -fsimdmath -ffp-model=strict" } */

/* Once per compilation.  The report is made from the front end's
   post_options, which runs once, rather than from finish_options, which
   runs again for every optimize attribute in the file whatever it is
   about - that placement needed a static guard and still let a transient
   pragma region latch the one report.  Extra copies land in excess
   errors, so this fails if it ever moves back.  */

__attribute__((optimize("O3"))) int f1 (void) { return 1; }
__attribute__((optimize("O3"))) int f2 (void) { return 2; }
__attribute__((optimize("O3"))) int f3 (void) { return 3; }

int
main (void)
{
  return f1 () + f2 () + f3 ();
}

/* { dg-regexp {.*warning: '-fsimdmath' has no effect while '-frounding-math' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: '-ffp-model=strict' turns on '-frounding-math'} } */
/* { dg-regexp {.*note: a vector variant does not follow the rounding mode, so no math call is vectorized} } */
