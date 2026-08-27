/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 -mno-daz-ftz -ffp-model=strict -funsafe-math-optimizations" } */
/* -mno-daz-ftz by name, not by default: this runs denormals through
   arithmetic, so a board that passes -mdaz-ftz would flush them and the
   run would fail on a compiler that is behaving correctly.  */

/* fp-model-20.c with the two options the other way round.  The flag-level
   arbitration cannot help here - options are handled in order, so the
   -funsafe-math-optimizations named last wins the compile - but the link
   is decided by a spec, which sees the whole command line at once.  So
   denormals survive whichever order the two were given in, which is the
   property that matters for a program built out of CFLAGS assembled by
   several makefiles.

   The compile side really is contradictory here and GCC says so: the
   model asked for signaling NaNs and the option that follows it asks for
   reassociation.  That warning is the correct answer to this command
   line, so the test expects it rather than pruning it away.  */

/* { dg-warning "'-fassociative-math' disabled" "" { target *-*-* } 0 } */

int
main (void)
{
  volatile double x = 0x1p-1022;
  volatile double y = 0x1p-52;
  double d = x * y;

  if (d == 0.0)
    __builtin_abort ();
  return 0;
}
