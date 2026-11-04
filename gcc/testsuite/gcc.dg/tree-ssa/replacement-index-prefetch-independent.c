/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

/* An available address part independent of OLD remains reusable.  */
unsigned long
independent_partial (unsigned *q, const unsigned char *table,
		     const unsigned *future, unsigned n, unsigned seed)
{
  unsigned high = (seed & 255u) << 16;
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i)
    {
      unsigned old = q[i & 7];
      unsigned next = future[i];
      q[i & 7] = next;
      sum += table[high + (old >> 8)];
    }
  return sum;
}

/* A load whose address is independent of OLD can be reused too.  No new
   load is needed: HIGH was computed before entering the loop.  */
unsigned long
independent_load (unsigned *q, const unsigned char *table,
		  const unsigned *future, const unsigned *lookup,
		  unsigned n, unsigned seed)
{
  unsigned high = lookup[seed & 15] << 16;
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i)
    {
      unsigned old = q[i & 7];
      unsigned next = future[i];
      q[i & 7] = next;
      sum += table[high + (old >> 8)];
    }
  return sum;
}

/* { dg-final { scan-tree-dump-times "Replacement-index prefetch:" 2 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times {next_[0-9]+ >> 8} 2 "aprefetch" } } */
