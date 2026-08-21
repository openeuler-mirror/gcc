/* { dg-do compile } */
/* { dg-options "-O2 -g -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

/* The rewrite gives four of the source's variables values the program
   never computed: three become zero because their contribution is now
   inside the high half, and the high quadrant product becomes the whole
   high half.  Their debug binds must be dropped rather than left
   reporting those values.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
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

/* { dg-final { scan-tree-dump "DEBUG carry => NULL" "mul_widen128" } } */
/* { dg-final { scan-tree-dump "DEBUG hh => NULL" "mul_widen128" } } */
