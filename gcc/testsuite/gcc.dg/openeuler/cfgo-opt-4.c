/* { dg-do compile } */
/* { dg-options "-O2 -fno-cfgo-profile-generate" } */

/* Same inversion as cfgo-opt-3.c, on the generate side, where it costs
   more: the handler continues into profile_arc_flag and friends, so an
   inverted value instruments the whole translation unit.  Asking for no
   profile generation must not emit calls into libgcov.  */

int g (int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    if (i & 1)
      s += i;
  return s;
}

/* { dg-final { scan-assembler-not "__gcov" } } */
