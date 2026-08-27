/* { dg-do compile } */
/* { dg-options "-ffp-model=fast -ffp-model=strict" } */

/* Only the last -ffp-model= takes effect: the earlier one is dropped by
   the driver with a warning, so fast-math must not be enabled.  */
#ifdef __FAST_MATH__
#error earlier -ffp-model=fast survived the override
#endif

int
main (void)
{
  return 0;
}

/* { dg-warning "overridden by" "" { target *-*-* } 0 } */
