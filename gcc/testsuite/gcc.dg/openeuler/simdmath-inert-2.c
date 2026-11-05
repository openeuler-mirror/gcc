/* { dg-do compile } */
/* { dg-options "-O2 -fsimdmath -ffp-model=strict" } */

/* strict and precise turn on -frounding-math, which a vector variant does
   not follow, so nothing at all is vectorized - the option is inert.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp {.*warning: '-fsimdmath' has no effect while '-frounding-math' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: '-ffp-model=strict' turns on '-frounding-math'} } */
/* { dg-regexp {.*note: a vector variant does not follow the rounding mode, so no math call is vectorized} } */
