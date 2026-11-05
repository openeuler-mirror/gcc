/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */

/* LL has to be the lo*lo quadrant.  This is the wrapping-addition
   formulation, where the check lives in match_add_lo_parts; the
   bit-splice one is mul-widen128-badll-3.c and the low-half-only one is
   mul-widen128-badll-1.c.  Here LL is ah*bl, so what the code computes
   is not the product at all, while the shape is otherwise a perfect
   schoolbook - every other guard accepts it.  With the check removed
   this folded to a plain widening multiply and gave the wrong answer on
   every input tried.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
bad_ll (uint64_t in0, uint64_t in1, uint64_t *hi, uint64_t *lo)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m01 = ah * bl, m11 = ah * bh;
  uint64_t ll = ah * bl;		/* not al * bl */
  uint64_t addc = m10 + m01;
  m11 += (uint64_t) (addc < m01) << 32;
  m11 += addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = ll + addc32;
  m11 += (uint64_t) (low < addc32);
  *lo = low;
  *hi = m11;
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook" "mul_widen128" } } */
/* { dg-final { scan-assembler-not {\tumulh\t} } } */
