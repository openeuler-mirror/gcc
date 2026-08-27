/* { dg-do compile } */
/* { dg-options "-O2 -fsimdmath -ffp-model=precise" } */

/* precise turns on -frounding-math just as strict does, so -fsimdmath is
   equally inert under it.  simdmath-inert-2/3/5 pin that for strict;
   this pins it for precise, whose -frounding-math half had no test.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp {.*warning: '-fsimdmath' has no effect while '-frounding-math' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: '-ffp-model=precise' turns on '-frounding-math'} } */
/* { dg-regexp {.*note: a vector variant does not follow the rounding mode, so no math call is vectorized} } */
