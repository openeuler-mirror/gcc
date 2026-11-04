/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

/* Do not keep an available old-dependent subexpression when its remainder
   cannot be rebuilt by the restricted hint arithmetic.  */
unsigned long
unsupported_partial (unsigned *q, const unsigned char *table,
		     const unsigned *future, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i)
    {
      unsigned old = q[i & 7];
      unsigned high = (old % 7u) << 16;
      q[i & 7] = future[i];
      sum += table[high + (old >> 8)];
    }
  return sum;
}

/* Rebuilding a partially available address must not introduce another
   memory load.  The load address, too, can depend on OLD.  */
unsigned long
loaded_partial (unsigned *q, const unsigned char *table,
		unsigned next, const unsigned *lookup, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i)
    {
      unsigned old = q[i & 7];
      unsigned high = lookup[old & 255u] << 16;
      q[i & 7] = next;
      sum += table[high + (old >> 8)];
    }
  return sum;
}

/* Only the lookup address can be rebuilt, not the final table address.
   Inspect its new index as well as the number of emitted hints.  */
/* { dg-final { scan-tree-dump-times "Replacement-index prefetch:" 1 "aprefetch" } } */
/* Address folding widens NEXT before applying the lookup-index mask.  */
/* { dg-final { scan-tree-dump-times {= \([^)]+\) next_[0-9]+\(D\)} 1 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times {__builtin_prefetch \(} 1 "aprefetch" } } */
