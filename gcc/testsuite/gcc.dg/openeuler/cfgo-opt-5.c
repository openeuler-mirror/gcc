/* { dg-do compile } */
/* { dg-options "-O2 -fcfgo-profile-use -Wno-missing-profile" } */

/* Counterpart to cfgo-opt-3.c: moving the value out of the shared case
   must leave the positive form working.  The bare form arrives with the
   value the driver set, so the set still turns on here.

   -Wno-missing-profile because the option also turns on the plain
   profile-use optimizations, which warn about the absent .gcda; that
   warning is the option working, not a defect.  */

#ifndef __GNUC_GNU_INLINE__
#error "-fcfgo-profile-use no longer enables the CFGO option set"
#endif

int h (int n)
{
  return n * 2;
}
