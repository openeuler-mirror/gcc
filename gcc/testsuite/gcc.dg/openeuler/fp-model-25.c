/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 -ffp-model=strict -mdaz-ftz -mno-daz-ftz" } */

/* -mno-daz-ftz is the override the feature was designed with.  It is not a
   priority though - the two spellings are one option, so prune_options
   keeps the last of them and -mno-daz-ftz -mdaz-ftz links the startup
   file.  Here it comes last, so the denormal survives.  */

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
