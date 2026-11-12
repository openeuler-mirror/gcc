/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-options "-O3 -fno-tree-pre -fno-tree-vectorize -fno-prefetch-loop-arrays -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

/* No candidate can emit a hint: modulo is outside the reconstruction
   subset.  The work limit must therefore apply independently of the
   number of successfully emitted hints.  Disable PRE so the queue loads
   remain inside the loop instead of becoming loop-header PHIs.  */
#define TRY(N) do { unsigned old = q[N]; q[N] = future[N]; \
                   sum += table[old % 7]; } while (0)
#define FOUR(N) TRY(N); TRY((N) + 1); TRY((N) + 2); TRY((N) + 3)
#define SIXTEEN(N) FOUR(N); FOUR((N) + 4); FOUR((N) + 8); FOUR((N) + 12)

unsigned long
failed_candidates (unsigned *q, const unsigned *future,
                   const unsigned char *table, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i)
    {
      SIXTEEN (0);
      SIXTEEN (16);
      SIXTEEN (32);
      SIXTEEN (48);
    }
  return sum;
}

/* { dg-final { scan-tree-dump "Replacement-index analysis budget exhausted" "aprefetch" } } */
/* { dg-final { scan-tree-dump-not "__builtin_prefetch" "aprefetch" } } */
