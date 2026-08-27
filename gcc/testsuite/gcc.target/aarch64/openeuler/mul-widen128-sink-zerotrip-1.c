/* { dg-do run } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertion below is a dump scan; see
   mul-widen128-run-1.c.  */

/* A high half stored twice: inside a loop that may run zero times, and
   again on a fallback path.  Unlike its two neighbours this is not the
   sinking gap, and re-anchoring the searches at plus_chain_root's block
   will not recover it: with two stores, reassoc and PRE hoist the shared
   partial sum `m11 + addchi' back into the low half's block with two
   uses, and same_plus_chain requires each high-half contribution to be
   consumed exactly once.  Measured: -fno-tree-sink recovers the two
   neighbours and leaves this one unmatched, and removing either store
   makes it match at plain -O2.

   The rewrite therefore falls back to folding the low half alone: a
   plain 64-bit multiply where a widening one would do.  The values are
   right either way, which is what the run half checks.  The dump
   assertion is xfail: it records the gap rather than hiding it.  Its two
   neighbours were xfail for the sinking reason and are not any more -
   this pass now runs ahead of pass_sink_code - but that move does not
   reach this shape; it pins the multiple-use half of the TODO above
   find_quadrant_mul.

   Shape taken from the generated corpus in the round-14 review; of its 52
   functions, 12 degrade this way, all of them in the three families this
   file and its two neighbours stand for.  */

typedef unsigned long long uint64_t;

#define M 0xffffffffULL

static void __attribute__((noipa))
sink_zerotrip (uint64_t in0, uint64_t in1, int n, uint64_t *hi, uint64_t *lo)
{
  uint64_t al = in0 & M, ah = in0 >> 32, bl = in1 & M, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  uint64_t midcarry = (uint64_t) (addc < m01) << 32;
  uint64_t addchi = addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  _Bool c = low < addc32;
  int i;

  *lo = low;
  for (i = 0; i < n; i++)
    hi[i] = 0;
  for (i = 0; i < n; i++)
    for (int j = 0; j < (n & 1); j++)
      hi[i] = m11 + addchi + midcarry + (uint64_t) c;
  for (i = 0; i < n; i++)
    if (!hi[i])
      hi[i] = m11 + addchi + midcarry + (uint64_t) c;
}

static const uint64_t v[] = {
  0, 1, 0xffffffffULL, 0x100000000ULL, 0xffffffffffffffffULL,
  0x8000000000000000ULL, 0xdeadbeefcafebabeULL, 0x123456789abcdefULL
};

int
main (void)
{
  unsigned i, j;
  uint64_t hi[4], lo;

  for (i = 0; i < sizeof v / sizeof v[0]; i++)
    for (j = 0; j < sizeof v / sizeof v[0]; j++)
      {
	unsigned __int128 p = (unsigned __int128) v[i] * v[j];
	unsigned k;

	sink_zerotrip (v[i], v[j], 4, hi, &lo);
	if (lo != (uint64_t) p)
	  __builtin_abort ();
	for (k = 0; k < 4; k++)
	  if (hi[k] != (uint64_t) (p >> 64))
	    __builtin_abort ();
      }
  return 0;
}

/* { dg-final { scan-tree-dump-times "rewrote schoolbook \\(" 1 "mul_widen128" { xfail *-*-* } } } */
