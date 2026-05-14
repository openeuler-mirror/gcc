/* { dg-do compile } */
/* { dg-options "-O2 -mschoolbook-widen -fdump-tree-schoolbook_widen-details" } */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
schoolbook_mul64_disconnected_hi (uint64_t x, uint64_t y, uint64_t z,
                                  uint64_t *lo, uint64_t *a, uint64_t *b,
                                  uint64_t *c, uint64_t *d)
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
  *a = hh + z;
  *b = (mid >> 32) + z;
  *c = (temp >> 32) + z;
  *d = carry + z;
}

/* { dg-final { scan-tree-dump-not "schoolbook_widen: rewrote schoolbook" "schoolbook_widen" } } */
