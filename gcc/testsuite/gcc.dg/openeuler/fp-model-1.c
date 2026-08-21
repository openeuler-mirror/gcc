/* { dg-do compile } */
/* { dg-options "-Ofast -ffp-model=precise" } */

/* Driver-level arbitration must degrade -Ofast to -O3 (with a warning
   emitted through a fully initialized diagnostic context), so no
   fast-math state may leak into the compilation.  */
#ifdef __FAST_MATH__
#error fast-math survived -ffp-model=precise
#endif

int
main (void)
{
  return 0;
}

/* { dg-warning "degraded to" "" { target *-*-* } 0 } */
