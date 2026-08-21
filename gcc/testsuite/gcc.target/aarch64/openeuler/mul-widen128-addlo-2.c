/* { dg-do compile } */
/* { dg-options "-O2 -mno-mul-widen128" } */

/* Without -mmul-widen128 nothing recognizes this shape, so the four
   quadrant products survive to the end of the GIMPLE pipeline.  A gated
   pass produces no dump file at all, so the assertion has to be made on
   the generated code, not on a mul_widen128 dump.  Say -mno-mul-widen128
   rather than relying on the default: sweeping the suite with
   --target_board=unix/-mmul-widen128 is a normal thing to do, and this
   is the one test in it that the option would otherwise turn red.  */

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
  m11 += (uint64_t) (low < addc32);
  *lo = low;
  *hi = m11;
}

/* { dg-final { scan-assembler-not {\tumulh\t} } } */
