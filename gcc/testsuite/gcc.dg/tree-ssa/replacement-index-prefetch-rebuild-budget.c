/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

/* Reconstructing HIGH, rather than reusing its old-index value, exceeds
   the arithmetic budget.  Reject the complete candidate in that case.  */
unsigned long
expensive_partial (unsigned *q, const unsigned char *table,
		   unsigned next, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i)
    {
      unsigned old = q[i & 7];
      unsigned high = ((old ^ 12345u) + (old >> 3)) ^ (old >> 5);
      q[i & 7] = next;
      sum += table[high + (old >> 8)];
    }
  return sum;
}

/* { dg-final { scan-tree-dump-not "Replacement-index prefetch:" "aprefetch" } } */
/* { dg-final { scan-tree-dump-not {__builtin_prefetch \(} "aprefetch" } } */
