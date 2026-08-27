/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details" } */

/* The four high-half contributions must reach one addition chain.  Here
   the carry out of the middle sum is stored separately instead, so the
   high half this code computes is not the high half of the product and
   must not be replaced by it.  The low half is still a valid low half,
   so that part may - and does - still fold.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
split_chain (uint64_t in0, uint64_t in1, uint64_t *hi, uint64_t *lo,
	     uint64_t *aside)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  uint64_t midcarry = (uint64_t) (m01 > addc) << 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  *aside = midcarry;			/* leaves the chain */
  *lo = low;
  *hi = m11 + (addc >> 32) + (uint64_t) (addc32 > low);
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook \\(" "mul_widen128" } } */
/* { dg-final { scan-assembler-not {\tumulh\t} } } */
