/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9.5-a+cmpbr -mtrack-speculation" } */

/* The speculation tracker requires every conditional jump to compare
   CC_REGNUM, which the CB<cc> forms do not, so +cmpbr has to give way to
   -mtrack-speculation.  */

int g (void);

int
f (int a, int b)
{
  if (a < b)
    return g ();
  return 0;
}

long
h (long a)
{
  if (a == 7)
    return g ();
  return 0;
}

/* { dg-final { scan-assembler-not "\tcb" } } */
