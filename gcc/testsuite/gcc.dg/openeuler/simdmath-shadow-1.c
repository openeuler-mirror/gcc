/* { dg-do compile } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-options "-fsimdmath" } */

/* simdmath.h is a header name that exists in the wild - the Cell SDK and
   libsimdmath both ship one - and -fsimdmath is meant to sit in a
   project's CFLAGS.  The pre-include is a system include, so a
   simdmath.h in the working directory must not be taken in place of the
   compiler's; when it was pushed as -include it was, and every
   translation unit of such a project silently got the wrong
   declarations.

   openeuler.exp puts a poisoned simdmath.h in the working directory
   around this one test; if it is ever consulted the #error in it fires
   here.  */

int
main (void)
{
  return 0;
}
