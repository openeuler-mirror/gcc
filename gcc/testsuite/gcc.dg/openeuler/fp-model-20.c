/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 -mno-daz-ftz -funsafe-math-optimizations -ffp-model=strict" } */
/* -mno-daz-ftz by name, not by default: this runs denormals through
   arithmetic, so a board that passes -mdaz-ftz would flush them and the
   run would fail on a compiler that is behaving correctly.  */

/* -funsafe-math-optimizations pulls in crtfastmath.o, which sets the
   flush-to-zero bit in the FPCR before main runs.  That is process-wide
   and no code generation choice undoes it, so a program that asked for
   strict IEEE behavior must not get it - even though the driver leaves
   the -funsafe-math-optimizations spelling on the command line.

   fp-model-21.c is this same program without the model, and does flush:
   without that pair, this test would also pass on a machine that never
   flushed anything.  */

int
main (void)
{
  volatile double x = 0x1p-1022;	/* smallest normal */
  volatile double y = 0x1p-52;
  double d = x * y;			/* denormal unless flushed */

  if (d == 0.0)
    __builtin_abort ();
  return 0;
}
