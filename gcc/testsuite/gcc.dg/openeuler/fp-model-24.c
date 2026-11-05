/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 -ffp-model=fast" } */

/* The positive direction of the same spec fp-model-20.c tests: fast is
   documented as equivalent to -ffast-math, so it has to pull in the
   startup file too, not merely fail to suppress it.  Only aarch64 named
   the model in its trigger list at first, which left the option meaning
   two different things on the two architectures this compiler ships for.
   Denormals are flushed, so the product below is zero.  */

int
main (void)
{
  volatile double x = 0x1p-1022;
  volatile double y = 0x1p-52;
  double d = x * y;

  if (d != 0.0)
    __builtin_abort ();
  return 0;
}
