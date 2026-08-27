/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 -mdaz-ftz" } */

/* -mdaz-ftz asks for flush-to-zero, which is set up by the startup file the
   link spec pulls in, so it is only observable in a running program: a
   product that would be denormal comes out as zero.  */

int
main (void)
{
  volatile double x = 0x1p-1022;	/* smallest normal */
  volatile double y = 0x1p-52;
  double d = x * y;			/* denormal without flush-to-zero */

  if (d != 0.0)
    __builtin_abort ();
  return 0;
}
