/* { dg-do compile } */
/* { dg-options "-O2 -fcfgo-profile-generate" } */

/* -fcfgo-profile-generate without "=" used to crash the compiler: the
   option handler fell through into the case that requires an argument
   and called xstrdup on a null pointer.  Compiling anything at all with
   the bare option is the whole test.  */

int f (int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    s += i;
  return s;
}
