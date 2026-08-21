/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details" } */

/* The carry added into the high half has to be the carry out of the low
   half's own addition.  Here it is a comparison against an unrelated
   value, so the high half depends on z and is not the high half of the
   product.  The low half is still a real low half and does fold, which
   is why the assertion names the full match specifically.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
bad_carry (uint64_t in0, uint64_t in1, uint64_t z, uint64_t *hi, uint64_t *lo)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  m11 += (uint64_t) (addc < m01) << 32;
  m11 += addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  m11 += (uint64_t) (low > z);	/* not this addition's carry */
  *lo = low;
  *hi = m11;
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook \\(" "mul_widen128" } } */
/* { dg-final { scan-assembler-not {\tumulh\t} } } */
