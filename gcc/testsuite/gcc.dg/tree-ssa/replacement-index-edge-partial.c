/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

/* Translating the join PHIs must also rebuild the available HIGH on the
   selected edge, rather than combining it with NEXT's low part.  */
__attribute__((noipa)) unsigned long
partial_edge (unsigned *q, const unsigned char *table, unsigned next,
	      unsigned other, unsigned choose, unsigned n)
{
  unsigned old, high;
  if (choose)
    {
      old = *q;
      high = (old & 255u) << 16;
      *q = next;
    }
  else
    {
      old = other;
      high = (other & 255u) << 16;
    }
  unsigned long tail = 0;
  for (unsigned i = 0; i < n; ++i)
    tail += table[i & 15];
  return table[high + (old >> 8)] + tail;
}

/* { dg-final { scan-tree-dump-times "Replacement-edge prefetch:" 1 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times {next_[0-9]+\(D\) << 16} 1 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times {next_[0-9]+\(D\) >> 8} 1 "aprefetch" } } */
