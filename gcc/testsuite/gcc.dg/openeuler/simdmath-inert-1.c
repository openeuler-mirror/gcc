/* { dg-do compile } */
/* { dg-options "-O2 -fsimdmath -ffp-model=except" } */

/* except keeps errno, and a vector variant cannot set errno per lane, so
   seven of the eleven functions stop being vectorized while four carry
   on.  That partial effect used to be entirely silent, which is the worst
   case of the three: the compiler really does emit some vector calls, so
   the option looks like it worked.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp {.*warning: '-fsimdmath' has no effect on the math functions that set 'errno' while '-fmath-errno' is in effect \[-Wsimdmath\]} } */
/* { dg-regexp {.*note: '-ffp-model=except' keeps 'errno'} } */
/* { dg-regexp {.*note: 'exp', 'expf', 'log', 'logf', 'pow', 'powf' and 'exp2f' are not vectorized; 'sin', 'sinf', 'cos' and 'cosf' still are} } */
