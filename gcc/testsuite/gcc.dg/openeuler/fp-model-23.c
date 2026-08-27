/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 -ffp-model=strict -mdaz-ftz" } */

/* An explicit -mdaz-ftz outranks the model.  This is the command line the
   feature was added for: its design note gives as the first motivation a
   user who wants flush-to-zero without the aggressive floating-point
   optimizations it is otherwise bundled with, and a value-safe model is
   exactly what such a user asks for alongside it.  Intel's -fp-model
   agrees - the value-safe models disable FTZ, but -fp-model precise -ftz
   is a documented combination.

   The model still does its own job here: the code generated for this
   file is IEEE-strict.  What -mdaz-ftz changes is the FPCR the program runs
   with, which is a separate request.  Denormals are therefore flushed
   and the product below is zero.

   fp-model-25.c pins the other end - that a -mno-daz-ftz still in force
   beats both.  The two spellings are one option, so between themselves
   the last one on the command line wins.  */

int
main (void)
{
  volatile double x = 0x1p-1022;	/* smallest normal */
  volatile double y = 0x1p-52;
  double d = x * y;

  if (d != 0.0)
    __builtin_abort ();
  return 0;
}
