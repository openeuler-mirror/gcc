/* { dg-do run } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

/* Differential test: both schoolbook formulations, compiled with the
   rewrite enabled, must agree with the target's own 128-bit multiply on
   the values where carries and half-boundaries actually bite.  */

typedef unsigned long long uint64_t;

/* Bit-splice low half.  */
static void __attribute__((noipa))
splice (uint64_t x, uint64_t y, uint64_t *hi, uint64_t *lo)
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

/* Wrapping-addition low half (bignum style), carries in the explicit
   comparison spelling - the `if' spelling is not recognized; that gap
   is mul-widen128-addlo-1.c's xfail.  */
static void __attribute__((noipa))
addlo (uint64_t in0, uint64_t in1, uint64_t *hi, uint64_t *lo)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  m11 += (uint64_t) (addc < m01) << 32;
  m11 += addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  m11 += (uint64_t) (low < addc32);
  *lo = low;
  *hi = m11;
}

static const uint64_t v[] = {
  0, 1, 2, 0xffffffffULL, 0x100000000ULL, 0xffffffffffffffffULL,
  0x8000000000000000ULL, 0x123456789abcdefULL, 0xfedcba987654321ULL,
  0x5555555555555555ULL, 0xaaaaaaaaaaaaaaaaULL
};

int
main (void)
{
  int n = sizeof v / sizeof v[0];
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      {
	uint64_t x = v[i], y = v[j], hi, lo;
	unsigned __int128 want = (unsigned __int128) x * y;
	splice (x, y, &hi, &lo);
	if (hi != (uint64_t) (want >> 64) || lo != (uint64_t) want)
	  __builtin_abort ();
	addlo (x, y, &hi, &lo);
	if (hi != (uint64_t) (want >> 64) || lo != (uint64_t) want)
	  __builtin_abort ();
      }
  return 0;
}

/* Both formulations must actually have been rewritten; otherwise this
   test would pass simply by not optimizing anything.  */
/* The assertion names the full match.  A bare "rewrote schoolbook" is
   also printed by the low-half-only fallback, so it cannot tell a lost
   full match from a working one - measured: dropping a carry
   alternative left this dump assertion passing and only the assembler
   one red.  */
/* { dg-final { scan-tree-dump-times "rewrote schoolbook \\(" 2 "mul_widen128" } } */
