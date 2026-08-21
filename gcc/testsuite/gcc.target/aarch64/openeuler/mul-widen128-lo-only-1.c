/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details" } */

/* Callers that only want the low 64 bits write the schoolbook without any
   high half at all.  The sum is still X*Y modulo 2^64, so it folds to a
   plain multiply.  */

typedef unsigned long long uint64_t;

uint64_t
lo_only (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xffffffffULL, xh = x >> 32;
  uint64_t yl = y & 0xffffffffULL, yh = y >> 32;
  return xl * yl + ((xl * yh + xh * yl) << 32);
}

/* { dg-final { scan-tree-dump-times "rewrote schoolbook low half" 1 "mul_widen128" } } */
/* { dg-final { scan-assembler-times {\tmul\t} 1 } } */
/* { dg-final { scan-assembler-not {\tumull\t} } } */
