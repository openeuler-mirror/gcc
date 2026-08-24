/* { dg-do compile } */
/* { dg-options "-O2 -fno-cfgo-profile-use" } */

/* The negative form must not turn the CFGO set on.  Neither bare option
   carries RejectNegative, so -fno- reaches the same handler as the
   positive form, and a value forced there rather than in the "=" case
   inverts it.  gnu89 inline is one of the options the set enables, and
   the preprocessor reports which inline semantics are in effect, so it
   serves as the observable.  */

#ifdef __GNUC_GNU_INLINE__
#error "-fno-cfgo-profile-use enabled the CFGO option set"
#endif

int f (int n)
{
  return n + 1;
}
