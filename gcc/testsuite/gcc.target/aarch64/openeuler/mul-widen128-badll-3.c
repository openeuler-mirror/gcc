/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */

/* The bit-splice formulation's own LL check.  mul-widen128-badll-1.c
   covers the low-half-only matcher and mul-widen128-badll-2.c the
   wrapping-addition one; try_match, which handles the shape below, had
   no test at all - removing its `LL must be lo*lo' guard left the suite
   green while every input produced the wrong answer.

   ll is x_hi * y_lo here rather than x_lo * y_lo, so the value assembled
   is not x * y.  Everything else is the schoolbook of
   mul-widen128-1.c.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
bad_ll_splice (uint64_t x, uint64_t y, uint64_t *hi, uint64_t *lo)
{
  const uint64_t mask = 0xffffffffULL;
  uint64_t x_lo = x & mask;
  uint64_t x_hi = x >> 32;
  uint64_t y_lo = y & mask;
  uint64_t y_hi = y >> 32;

  uint64_t ll = x_hi * y_lo;		/* not x_lo * y_lo */
  uint64_t lh = x_lo * y_hi;
  uint64_t hl = x_hi * y_lo;
  uint64_t hh = x_hi * y_hi;
  uint64_t mid = lh + hl;
  uint64_t carry = (uint64_t) (hl > mid) << 32;
  uint64_t temp = (ll >> 32) + (mid & mask);

  *lo = (temp << 32) | (ll & mask);
  *hi = hh + (mid >> 32) + (temp >> 32) + carry;
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook" "mul_widen128" } } */
/* { dg-final { scan-assembler-not {\tumulh\t} } } */
