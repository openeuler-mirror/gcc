/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-options "-O3 -ffp-model=except -fsimdmath" } */

/* -fsimdmath turns errno reporting off so that math calls can be
   vectorized, but the exception-preserving models promise errno, and a
   later -fsimdmath must not quietly take that promise away.  */
#ifndef __NO_MATH_ERRNO__
/* errno is still on: this is what the model asked for.  */
#else
#error -fsimdmath cleared errno under -ffp-model=except
#endif

int
main (void)
{
  return 0;
}


/* The option is now diagnosed where errno keeps part of it from
   working; this test is one of the command lines that triggers it.  */
/* { dg-regexp {.*warning: '-fsimdmath' has no effect on the math functions that set 'errno' while '-fmath-errno' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: '-ffp-model=except' keeps 'errno'} } */
/* { dg-regexp {.*note: 'exp', 'expf', 'log', 'logf', 'pow', 'powf' and 'exp2f' are not vectorized; 'sin', 'sinf', 'cos' and 'cosf' still are} } */
