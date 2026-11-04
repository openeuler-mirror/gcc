/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

/* HIGH is available at the replacement store but still depends on OLD.
   Both halves of the hinted address must use NEXT, even if one half was
   computed before the insertion point.  With old = 0 and next = 1 the
   correct hinted offset is 65536, not 0.  */
__attribute__((noipa)) unsigned long
partial_loop (unsigned *q, const unsigned char *table,
	      const unsigned *future, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i)
    {
      unsigned old = q[i & 7];
      unsigned high = (old & 255u) << 16;
      unsigned next = future[i];
      q[i & 7] = next;
      sum += table[high + (old >> 8)];
    }
  return sum;
}

/* { dg-final { scan-tree-dump-times "Replacement-index prefetch:" 1 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times {next_[0-9]+ << 16} 1 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times {next_[0-9]+ >> 8} 1 "aprefetch" } } */
