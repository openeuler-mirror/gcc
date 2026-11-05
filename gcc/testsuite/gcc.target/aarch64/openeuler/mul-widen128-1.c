/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details" } */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
schoolbook_mul64 (uint64_t x, uint64_t y, uint64_t *hi, uint64_t *lo)
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

  *lo = (temp << 32) | (ll & mask);
  *hi = hh + (mid >> 32) + (temp >> 32) + carry;
}

/* The assertion names the full match.  A bare "rewrote schoolbook" is
   also printed by the low-half-only fallback, so it cannot tell a lost
   full match from a working one.  */
/* { dg-final { scan-tree-dump-times "rewrote schoolbook \\(" 1 "mul_widen128" } } */
/* { dg-final { scan-assembler-times {\tumulh\t} 1 } } */
