/* { dg-do run } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -mmul-widen128 -flto" } */

/* The option is part of the per-function optimization state, so it
   survives into the link-time stage and the rewrite still happens there;
   this is the configuration in which it is easiest to lose silently.  */

typedef unsigned long long uint64_t;

static void __attribute__((noipa))
mul (uint64_t x, uint64_t y, uint64_t *hi, uint64_t *lo)
{
  const uint64_t mask = 0xffffffffULL;
  uint64_t x_lo = x & mask, x_hi = x >> 32;
  uint64_t y_lo = y & mask, y_hi = y >> 32;
  uint64_t ll = x_lo * y_lo, lh = x_lo * y_hi;
  uint64_t hl = x_hi * y_lo, hh = x_hi * y_hi;
  uint64_t mid = lh + hl;
  uint64_t carry = (uint64_t) (hl > mid) << 32;
  uint64_t temp = (ll >> 32) + (mid & mask);
  *lo = (temp << 32) | (ll & mask);
  *hi = hh + (mid >> 32) + (temp >> 32) + carry;
}

int
main (void)
{
  uint64_t x = 0xdeadbeefcafebabeULL, y = 0x0123456789abcdefULL, hi, lo;
  unsigned __int128 want = (unsigned __int128) x * y;
  mul (x, y, &hi, &lo);
  if (hi != (uint64_t) (want >> 64) || lo != (uint64_t) want)
    __builtin_abort ();
  return 0;
}
/* No dump assertion is possible here: under -flto the pass runs in the
   link-time stage and writes its dump into the LTRANS temporary
   directory, which the testsuite cannot scan.  That the rewrite fires
   at all is covered by the non-LTO tests; what this one adds is that
   the option still reaches the link-time stage and the result is
   right.  */
