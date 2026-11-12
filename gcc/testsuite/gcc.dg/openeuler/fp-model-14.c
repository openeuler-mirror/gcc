/* { dg-do compile } */
/* { dg-options "-ffp-model=strict -ffast-math" } */

/* -Ofast after a precise or strict model is degraded to -O3 with a
   warning.  -ffast-math is the same request spelled differently, so it
   has to be treated the same way; it used to be cancelled only when it
   came before the model.  */
#ifdef __FAST_MATH__
#error -ffast-math after -ffp-model=strict was not cancelled
#endif

int
main (void)
{
  return 0;
}

/* { dg-warning "'-ffast-math' after '-ffp-model=strict' is canceled" "" { target *-*-* } 0 } */
