/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

/* The shifted addend has to be the sum of the two cross products.  Here
   it is an unrelated sum, computed in the same block so that the match
   fails on that ground and not because the value has no definition.  */

typedef unsigned long long uint64_t;

uint64_t __attribute__((noipa))
bad_mid (uint64_t x, uint64_t y, uint64_t z, uint64_t w)
{
  uint64_t xl = x & 0xffffffffULL, yl = y & 0xffffffffULL;
  uint64_t m = z + w;
  return xl * yl + (m << 32);
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook" "mul_widen128" } } */
