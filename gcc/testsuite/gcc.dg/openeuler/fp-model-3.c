/* { dg-do compile } */
/* { dg-options "-ffast-math -ffp-model=precise" } */

/* An -ffast-math preceding -ffp-model=precise is canceled by the driver
   with a warning.  */
#ifdef __FAST_MATH__
#error -ffast-math before -ffp-model=precise survived
#endif

int
main (void)
{
  return 0;
}

/* { dg-warning "is canceled" "" { target *-*-* } 0 } */
