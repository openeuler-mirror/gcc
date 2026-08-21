/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details" } */

/* An unsigned sum wraps exactly when it lands below either addend, so the
   carry out of the low half is just as often written against the product
   as against the shifted middle term.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
bn_mul64 (uint64_t in0, uint64_t in1, uint64_t *hi, uint64_t *lo)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  m11 += (uint64_t) (addc < m01) << 32;
  m11 += addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  m11 += (uint64_t) (low < m00);	/* against the other addend */
  *lo = low;
  *hi = m11;
}

/* The assertion names the full match.  A bare "rewrote schoolbook" is
   also printed by the low-half-only fallback, so it cannot tell a lost
   full match from a working one - measured: dropping a carry
   alternative left this dump assertion passing and only the assembler
   one red.  */
/* { dg-final { scan-tree-dump-times "rewrote schoolbook \\(" 1 "mul_widen128" } } */
/* { dg-final { scan-assembler-times {\tumulh\t} 1 } } */
