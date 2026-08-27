/* { dg-do compile } */
/* { dg-options "-ffp-model=fast -fno-fast-math" } */

/* The fast model is the one case where the model yields: an explicit
   -fno-fast-math after it cancels it, with a warning.  This branch of the
   driver arbitration had no test.  */
#ifdef __FAST_MATH__
#error -fno-fast-math after -ffp-model=fast did not cancel it
#endif

int
main (void)
{
  return 0;
}

/* { dg-warning "is canceled" "" { target *-*-* } 0 } */
