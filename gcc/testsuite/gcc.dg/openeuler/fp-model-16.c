/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-options "-fmath-errno -fsimdmath" } */

/* An explicit -fmath-errno is a request, not a default, so -fsimdmath
   leaves it alone even with no floating-point model in play.  This is
   the half of the guard that keys on opts_set rather than on the
   model.  */
#ifdef __NO_MATH_ERRNO__
#error -fsimdmath discarded an explicit -fmath-errno
#endif

int
main (void)
{
  return 0;
}


/* The option is now diagnosed where errno keeps part of it from
   working; this test is one of the command lines that triggers it.  */
/* { dg-regexp {.*warning: '-fsimdmath' has no effect on the math functions that set 'errno' while '-fmath-errno' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: 'exp', 'expf', 'log', 'logf', 'pow', 'powf' and 'exp2f' are not vectorized; 'sin', 'sinf', 'cos' and 'cosf' still are} } */
