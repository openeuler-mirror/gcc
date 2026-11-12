/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-Ofast -mno-daz-ftz" } */

/* -Ofast turns flush-to-zero on; -mno-daz-ftz is the only way to keep
   denormals while keeping the rest of fast-math, so it has to win.  */

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
