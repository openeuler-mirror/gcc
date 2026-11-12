/* { dg-do compile } */
/* { dg-options "-O2 -g -fcompare-debug -fno-tree-vectorize -fno-prefetch-loop-arrays -ftree-replacement-index-prefetch" } */
unsigned long
f1 (unsigned *q, unsigned char *table, const unsigned *future, unsigned n)
{
unsigned long sum = 0;
for (unsigned i = 0; i < n; ++i)
  {
    unsigned old = q[i & 7];
    unsigned a = future[i];
    unsigned b = a ^ (a >> 7);
    q[i & 7] = b;
    sum += table[old];
  }
return sum;
}
