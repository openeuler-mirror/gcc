/* { dg-do run } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertion below is a dump scan; see
   mul-widen128-run-1.c.  */

/* The low-half-only rewrite had no run test: mul-widen128-lo-only-1.c is
   compile-only and the one executable in that area is a negative.  A
   rewrite that folds three quadrant products into one multiply is worth
   checking against the answer, not just against the dump.

   Three spellings of "only the low 64 bits", over the values where the
   carries and the half-boundaries bite.  */

typedef unsigned long long uint64_t;

static uint64_t __attribute__((noipa))
lo_only (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xffffffffULL, xh = x >> 32;
  uint64_t yl = y & 0xffffffffULL, yh = y >> 32;
  return xl * yl + ((xl * yh + xh * yl) << 32);
}

static uint64_t __attribute__((noipa))
lo_only_swapped (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xffffffffULL, xh = x >> 32;
  uint64_t yl = y & 0xffffffffULL, yh = y >> 32;
  return ((xh * yl + xl * yh) << 32) + xl * yl;
}

static uint64_t __attribute__((noipa))
lo_only_splice (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xffffffffULL, xh = x >> 32;
  uint64_t yl = y & 0xffffffffULL, yh = y >> 32;
  uint64_t mid = xl * yh + xh * yl;
  return (xl * yl & 0xffffffffULL) | ((xl * yl >> 32) + mid) << 32;
}

static const uint64_t v[] = {
  0, 1, 2, 0xffffffffULL, 0x100000000ULL, 0x100000001ULL,
  0xffffffffffffffffULL, 0xfffffffeULL, 0x7fffffffffffffffULL,
  0x8000000000000000ULL, 0xdeadbeefcafebabeULL, 0x123456789abcdefULL
};

int
main (void)
{
  unsigned i, j;

  for (i = 0; i < sizeof v / sizeof v[0]; i++)
    for (j = 0; j < sizeof v / sizeof v[0]; j++)
      {
	uint64_t want = (uint64_t) (v[i] * v[j]);

	if (lo_only (v[i], v[j]) != want)
	  __builtin_abort ();
	if (lo_only_swapped (v[i], v[j]) != want)
	  __builtin_abort ();
	if (lo_only_splice (v[i], v[j]) != want)
	  __builtin_abort ();
      }
  return 0;
}

/* Two of the three are recognized.  lo_only_splice is not: it reads
   xl * yl twice - once masked, once shifted - and the matcher requires
   each contribution to have a single use, so it declines and leaves the
   function alone.  It stays here as a correctness control: whether or
   not it is rewritten, it has to give the same answer.  */
/* { dg-final { scan-tree-dump-times "rewrote schoolbook low half" 2 "mul_widen128" } } */
