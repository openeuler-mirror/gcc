/* { dg-do link { target aarch64*-*-* } } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-require-effective-target libmathlib } */
/* { dg-options "-O3 -fsimdmath -lm" } */

/* The documented contract followed: with -lm on the line the link
   succeeds.  Without this the test next door would pass against a driver
   that could not link this program under any command line.  */

#include <math.h>

double a[1024], b[1024];

int
main (void)
{
  int i;

  for (i = 0; i < 1024; i++)
    a[i] = pow (a[i], b[i]);
  return a[0] > 1.0;
}
