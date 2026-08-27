/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

/* The addend that is not shifted has to be the low quadrant product of
   the same pair.  Here it is an unrelated product, so the sum is not the
   low half of x*y and must be left alone.  */

typedef unsigned long long uint64_t;

uint64_t __attribute__((noipa))
bad_ll (uint64_t x, uint64_t y, uint64_t p, uint64_t q)
{
  uint64_t xl = x & 0xffffffffULL, xh = x >> 32;
  uint64_t yl = y & 0xffffffffULL, yh = y >> 32;
  return p * q + ((xl * yh + xh * yl) << 32);
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook" "mul_widen128" } } */
