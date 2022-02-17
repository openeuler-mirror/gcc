/* { dg-do compile } */
/* { dg-options "-O2 -fprefetch-loop-arrays=2 --param min-insn-to-prefetch-ratio=5 --param simultaneous-prefetches=100 -fdump-tree-aprefetch-details -fdump-tree-optimized" } */
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
/* { dg-final { scan-tree-dump-times "Ahead 40" 1 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times "builtin_prefetch" 1 "optimized" } } */