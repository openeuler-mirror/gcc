/* { dg-do run } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertions below are dump scans.  The
   dg layer adds it automatically for scan-assembler* under an -flto
   board but not for scan-tree-dump*, so without it the dump lands in
   LTRANS, the scan finds no file, and the test reports UNRESOLVED
   rather than either answer - 15 of them did.  */

/* Callers that accumulate only the high half let the low half die.  The
   matcher for that case had no test at all.  */

typedef unsigned long long uint64_t;

static uint64_t __attribute__((noipa))
hi_only (uint64_t x, uint64_t y)
{
  const uint64_t mask = 0xffffffffULL;
  uint64_t x_lo = x & mask, x_hi = x >> 32;
  uint64_t y_lo = y & mask, y_hi = y >> 32;
  uint64_t ll = x_lo * y_lo, lh = x_lo * y_hi;
  uint64_t hl = x_hi * y_lo, hh = x_hi * y_hi;
  uint64_t mid = lh + hl;
  uint64_t carry = (uint64_t) (hl > mid) << 32;
  uint64_t temp = (ll >> 32) + (mid & mask);
  return hh + (mid >> 32) + (temp >> 32) + carry;
}

static const uint64_t v[] = {
  0, 1, 0xffffffffULL, 0x100000000ULL, 0x8000000000000000ULL,
  0xffffffffffffffffULL, 0x123456789abcdefULL
};

int
main (void)
{
  int n = sizeof v / sizeof v[0];
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      {
	unsigned __int128 want = (unsigned __int128) v[i] * v[j];
	if (hi_only (v[i], v[j]) != (uint64_t) (want >> 64))
	  __builtin_abort ();
      }
  return 0;
}

/* { dg-final { scan-tree-dump "HI-only match" "mul_widen128" } } */
