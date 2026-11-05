/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */

/* The same alternative one level up.  The middle sum `mid = LH + HL'
   wraps exactly when it lands below either cross product, so its carry
   is written against whichever one the author had to hand.
   mul-widen128-carry-alt-1.c covers the low half's two spellings; this
   covers the middle sum's, which had no test at all.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
bn_mul64 (uint64_t in0, uint64_t in1, uint64_t *hi, uint64_t *lo)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  m11 += (uint64_t) (addc < m10) << 32;	/* against the other cross product */
  m11 += addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  m11 += (uint64_t) (low < addc32);
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
