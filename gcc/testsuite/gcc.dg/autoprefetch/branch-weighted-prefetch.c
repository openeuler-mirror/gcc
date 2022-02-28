/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O2 -fprefetch-loop-arrays=2 --param min-insn-to-prefetch-ratio=5 --param simultaneous-prefetches=100 --param l1-cache-size=64 --param l1-cache-line-size=32 -fdump-tree-aprefetch-details -fdump-tree-optimized" } */
#define N 10000000

long long a[N];

long long func ()
{
  long long i;
  long long sum = 0;

  for (i = 0; i < N; i+=1) {
	if (i < 100000)
		sum += a[i];
	else
		continue;
  }

  return sum;
}
/* { dg-final { scan-tree-dump "Calculating prefetch distance using bb branch weighting method" "aprefetch" } } */
/* { dg-final { scan-tree-dump "builtin_prefetch" "optimized" } } */