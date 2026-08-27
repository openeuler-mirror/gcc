/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
schoolbook_mul64_extra_use (uint64_t x, uint64_t y, uint64_t *hi,
                            uint64_t *lo, uint64_t *extra)
{
  const uint64_t mask = 0xffffffffULL;
  uint64_t x_lo = x & mask;
  uint64_t x_hi = x >> 32;
  uint64_t y_lo = y & mask;
  uint64_t y_hi = y >> 32;

  uint64_t ll = x_lo * y_lo;
  uint64_t lh = x_lo * y_hi;
  uint64_t hl = x_hi * y_lo;
  uint64_t hh = x_hi * y_hi;
  uint64_t mid = lh + hl;
  uint64_t carry = (uint64_t) (hl > mid) << 32;
  uint64_t temp = (ll >> 32) + (mid & mask);

  *extra = hh;
  *lo = (temp << 32) | (ll & mask);
  *hi = hh + (mid >> 32) + (temp >> 32) + carry;
}

/* The extra use of hh means zeroing it would be wrong, so the full
   match must not fire.  The assertion names the full match: the low
   half is a genuine low half (verified by running it), so a future
   low-half-only fold would be legitimate and must not turn this red.  */
/* { dg-final { scan-tree-dump-not "rewrote schoolbook \\(" "mul_widen128" } } */
