/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details" } */

/* A schoolbook written entirely in signed types, with no casts anywhere
   to stop the match earlier.  `>> 32` is arithmetic here, so this is not
   the unsigned decomposition the rewrite assumes; only the signedness
   check stands between it and a wrong answer.  */

typedef long long int64_t;

void __attribute__((noipa))
sgn2 (int64_t x, int64_t y, int64_t *hi, int64_t *lo)
{
  int64_t xl = x & 0xffffffffLL, xh = x >> 32;
  int64_t yl = y & 0xffffffffLL, yh = y >> 32;
  int64_t ll = xl * yl, lh = xl * yh, hl = xh * yl, hh = xh * yh;
  int64_t mid = lh + hl;
  int64_t carry = (int64_t) (hl > mid) << 32;
  int64_t temp = (ll >> 32) + (mid & 0xffffffffLL);
  *lo = (temp << 32) | (ll & 0xffffffffLL);
  *hi = hh + (mid >> 32) + (temp >> 32) + carry;
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook" "mul_widen128" } } */
/* { dg-final { scan-assembler-not {\tsmulh\t} } } */
/* { dg-final { scan-assembler-not {\tumulh\t} } } */
