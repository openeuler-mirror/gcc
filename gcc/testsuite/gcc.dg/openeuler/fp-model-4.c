/* { dg-do compile } */
/* { dg-options "-Ofast -ffp-model=except" } */

/* -ffp-model=except asks for exception semantics, which fast-math
   destroys (-fno-trapping-math, -fno-signaling-nans).  The driver must
   arbitrate it like precise and strict: degrade -Ofast to -O3 so no
   fast-math state reaches the compilation.  */
#ifdef __FAST_MATH__
#error fast-math survived -ffp-model=except
#endif

int
main (void)
{
  return 0;
}

/* { dg-warning "degraded to" "" { target *-*-* } 0 } */
