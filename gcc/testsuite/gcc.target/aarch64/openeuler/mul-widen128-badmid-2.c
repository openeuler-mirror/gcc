/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

/* The middle term has to be the sum of the two cross products.  A
   difference of the same two products is a different number, and the
   low-half-only rewrite has no carry or chain check to fall back on -
   the shape of the middle term is the whole argument for it.  */

typedef unsigned long long uint64_t;

uint64_t __attribute__((noipa))
bad_minus (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xffffffffULL, xh = x >> 32;
  uint64_t yl = y & 0xffffffffULL, yh = y >> 32;
  return xl * yl + ((xl * yh - xh * yl) << 32);
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook" "mul_widen128" } } */
