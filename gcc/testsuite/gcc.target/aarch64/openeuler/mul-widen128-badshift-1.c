/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */

/* The middle carry is worth exactly 2^32 in the high half.  Here it is
   shifted by 33, so the value computed is not the product.  With the
   shift constant unchecked this folded anyway and was wrong on 14511 of
   200000 random operand pairs - the ones where the middle addition
   actually carried.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
bad_shift (uint64_t in0, uint64_t in1, uint64_t *hi, uint64_t *lo)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  m11 += (uint64_t) (addc < m01) << 33;	/* not 1 << 32 */
  m11 += addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  m11 += (uint64_t) (low < addc32);
  *lo = low;
  *hi = m11;
}

/* The low half is still x*y modulo 2^64 whatever the high half does, so
   it folds on its own; the assertion names the full match, as
   mul-widen128-badcarry-1.c does.  */
/* { dg-final { scan-tree-dump-not "rewrote schoolbook \\(" "mul_widen128" } } */
/* { dg-final { scan-assembler-not {\tumulh\t} } } */
