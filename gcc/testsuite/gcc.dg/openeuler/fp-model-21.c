/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 -funsafe-math-optimizations" } */

/* The control for fp-model-20.c: the same program, the same option, no
   model.  crtfastmath.o is linked and the product is flushed to zero.
   If this one ever stops flushing, fp-model-20.c has stopped proving
   anything and both need looking at.  */

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
