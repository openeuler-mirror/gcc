/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fdump-tree-aprefetch-details" } */
/* A queue replacement already computes the future indirect index. */
unsigned long replacement(unsigned *q, unsigned char *table,
			  const unsigned *future, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i) {
    unsigned old = q[i & 7];
    q[i & 7] = future[i];
    sum += table[(old >> 3) << 5];
  }
  return sum;
}
unsigned long different(unsigned *q, unsigned char *table,
			const unsigned *future, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i) {
    unsigned old = q[i & 7];
    q[(i+1) & 7] = future[i];
    sum += table[old];
  }
  return sum;
}
unsigned long volatile_queue(volatile unsigned *q, unsigned char *table,
			     const unsigned *future, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i) {
    unsigned old = q[i & 7];
    q[i & 7] = future[i];
    sum += table[old];
  }
  return sum;
}
unsigned long counter(unsigned *q, unsigned char *table, unsigned n)
{
  unsigned long sum=0;
  for(unsigned i=0;i<n;i++) {
    unsigned old=q[i&7]; q[i&7]=old+1;
    sum+=table[old];
  }
  return sum;
}

/* { dg-final { scan-tree-dump-times "Replacement-index prefetch:" 1 "aprefetch" } } */
