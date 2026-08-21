/* { dg-do run } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */
/* -ffat-lto-objects because the assertion below is a dump scan; see
   mul-widen128-run-1.c.  */

/* find_lo_carry requires the carry comparison to be in the block being
   rewritten, but not the widening conversion it feeds.  This is the
   shape that needs the distinction: the comparison is kept as _Bool and
   only widened where the high half is used, which is inside a loop, so
   pass_sink_code - which runs before this pass - moves the conversion
   into the loop preheader and leaves the comparison behind.

   Requiring both in one block loses this match outright, which is why
   the requirement was tried and reverted.  The reason given for it -
   that it keeps claim_stmts from seeing a statement outside the block -
   does not hold: claim_stmts compares pointers, rewrite_match reaches
   each statement through gsi_for_stmt, which resolves in that
   statement's own block, and SSA guarantees the anchor block dominates
   wherever the conversion was sunk to.

   The earlier test of this name asserted the same thing with an `if',
   and was vacuous: both arms were textually identical, so the diamond
   collapsed to a single basic block long before the pass ran.  With a
   loop the split survives.

   dg-do run, not compile: the point of accepting a sunk conversion is
   that the rewrite is still correct, so the answer is checked too.  */

typedef unsigned long long uint64_t;

#define M 0xffffffffULL

static void __attribute__((noipa))
xbb_loop_hi (uint64_t in0, uint64_t in1, int n, uint64_t *hi, uint64_t *lo)
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

	xbb_loop_hi (v[i], v[j], 4, hi, &lo);
	if (lo != (uint64_t) p)
	  __builtin_abort ();
	for (k = 0; k < 4; k++)
	  if (hi[k] != (uint64_t) (p >> 64))
	    __builtin_abort ();
      }
  return 0;
}

/* { dg-final { scan-tree-dump-times "rewrote schoolbook \\(" 1 "mul_widen128" } } */
