/* { dg-do link { target aarch64*-*-* } } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-require-effective-target libmathlib } */
/* { dg-options "-O3 -fsimdmath" } */

/* The packaged libmathlib is built without -lm and leaves its own
   references to the math library undefined, so a C link needs -lm on the
   command line.  The driver does not add one and does not say so either:
   an injected -lm would sit after the user's objects and satisfy their
   references too, so dropping -fsimdmath later would break a link that
   had been working, and a driver-side advisory cannot tell the links that
   need it from the ones that do not.  invoke.texi documents the
   requirement; this pins the failure that follows from not meeting it.

   Pin the reason, not just the failure: a bare dg-excess-errors passes on
   any link error at all, including one from a broken test.  Both halves
   are needed - the linker's message is attributed to no line, so
   dg-regexp matches the output directly, and dg-excess-errors absorbs the
   rest of the failure.

   openeuler.exp clears the board's mathlib for this directory; otherwise
   DejaGnu appends -lm itself and the link would never fail.
   simdmath-link-c-2.c is the same program with -lm, which links clean.  */

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

/* { dg-regexp {undefined reference to `[a-z_]+'} } */
/* { dg-excess-errors "the link fails without -lm, which is the point" } */
