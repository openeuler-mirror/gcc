/* { dg-do run } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

/* Squaring is not recognized, and this test says so rather than leaving
   the question open.  The two cross products are the same value, so CSE
   merges them and folds `lh + hl' into `lh * 2'; the middle term is then
   a multiply, not the sum the matchers require.  The result still has to
   be right - that is what the run half checks - it is just computed the
   way the source wrote it.  Teaching the matchers this shape would be a
   capability change, not a fix.  */

typedef unsigned long long uint64_t;

static void __attribute__((noipa))
square (uint64_t x, uint64_t *hi, uint64_t *lo)
{
  const uint64_t mask = 0xffffffffULL;
  uint64_t x_lo = x & mask, x_hi = x >> 32;
  uint64_t ll = x_lo * x_lo, lh = x_lo * x_hi;
  uint64_t hl = x_hi * x_lo, hh = x_hi * x_hi;
  uint64_t mid = lh + hl;
  uint64_t carry = (uint64_t) (hl > mid) << 32;
  uint64_t temp = (ll >> 32) + (mid & mask);
  *lo = (temp << 32) | (ll & mask);
  *hi = hh + (mid >> 32) + (temp >> 32) + carry;
}

static const uint64_t v[] = {
  0, 1, 2, 0xffffffffULL, 0x100000000ULL, 0x8000000000000000ULL,
  0xffffffffffffffffULL, 0x123456789abcdefULL, 0xaaaaaaaaaaaaaaaaULL
};

int
main (void)
{
  for (unsigned i = 0; i < sizeof v / sizeof v[0]; i++)
    {
      uint64_t hi, lo;
      unsigned __int128 want = (unsigned __int128) v[i] * v[i];
      square (v[i], &hi, &lo);
      if (hi != (uint64_t) (want >> 64) || lo != (uint64_t) want)
	__builtin_abort ();
    }
  return 0;
}

/* The assertion names the full match: the low half here is a genuine
   low half of x*x, so a future low-half-only fold of it would be
   legitimate and must not turn this test red.  What squaring must never
   get is the widening rewrite.  */
/* { dg-final { scan-tree-dump-not "rewrote schoolbook \\(" "mul_widen128" } } */
