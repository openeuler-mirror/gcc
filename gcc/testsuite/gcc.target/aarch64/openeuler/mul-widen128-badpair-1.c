/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

/* Both cross products have to come from the same pair of operands.  Here
   the second one is built from two unrelated values, so the sum is not
   the middle term of x*y and the low half is not x*y either.  */

typedef unsigned long long uint64_t;

uint64_t __attribute__((noipa))
bad_pair (uint64_t x, uint64_t y, uint64_t p, uint64_t q)
{
  uint64_t xl = x & 0xffffffffULL;
  uint64_t yl = y & 0xffffffffULL, yh = y >> 32;
  uint64_t pl = p & 0xffffffffULL, qh = q >> 32;
  return xl * yl + ((xl * yh + pl * qh) << 32);
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook" "mul_widen128" } } */
