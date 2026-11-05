/* { dg-do compile } */
/* { dg-options "-O2 -fcfgo-profile-generate=cfgo-opt-2.d" } */

/* The form with "=" takes a directory for the profile data and must keep
   working next to the bare form pinned by cfgo-opt-1.c.  */

int g (int n)
{
  return n * 3;
}
