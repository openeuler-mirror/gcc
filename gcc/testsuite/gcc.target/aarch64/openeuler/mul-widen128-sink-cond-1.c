/* { dg-do run } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertion below is a dump scan; see
   mul-widen128-run-1.c.  */

/* A high half stored under a single condition.

   All four matchers look for the schoolbook's parts in one basic block,
   anchored at the block holding the low half, and pass_sink_code moves
   the high half's work into whichever block consumes it.  With this pass
   downstream of sinking, a high half stored on only one path was not
   found and the rewrite fell back to folding the low half alone: a plain
   64-bit multiply where a widening one would do.

   That was the state until this pass moved ahead of pass_sink_code:
   the shape now matches, and the assertion below is a plain one.  It is
   kept as a guard - move the pass back downstream of sinking and it goes
   red again.

   Shape taken from the generated corpus in the round-14 review; of its 52
   functions, 12 degrade this way, all of them in the three families this
   file and its two neighbours stand for.  */

typedef unsigned long long uint64_t;

#define M 0xffffffffULL

static void __attribute__((noipa))
sink_cond (uint64_t in0, uint64_t in1, int n, uint64_t *hi, uint64_t *lo)
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
  if (n > 0)
    hi[0] = m11 + addchi + midcarry + (uint64_t) c;
  for (i = 1; i < n; i++)
    hi[i] = hi[0];
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

	sink_cond (v[i], v[j], 4, hi, &lo);
	if (lo != (uint64_t) p)
	  __builtin_abort ();
	for (k = 0; k < 4; k++)
	  if (hi[k] != (uint64_t) (p >> 64))
	    __builtin_abort ();
      }
  return 0;
}

/* { dg-final { scan-tree-dump-times "rewrote schoolbook \\(" 1 "mul_widen128" } } */
