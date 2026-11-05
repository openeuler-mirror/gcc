/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -ftree-replacement-index-prefetch" } */
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

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdatomic.h>
extern unsigned long replacement(unsigned *, unsigned char *, const unsigned *, unsigned);
extern unsigned long different(unsigned *, unsigned char *, const unsigned *, unsigned);
extern unsigned long volatile_queue(volatile unsigned *, unsigned char *, const unsigned *, unsigned);
__attribute__((noinline,noclone))
unsigned long variable(unsigned *q, unsigned char *table, const unsigned *v,
		       unsigned n, unsigned shift)
{
  unsigned long sum=0;
  for(unsigned i=0;i<n;i++) {
    unsigned x=q[i&7]; q[i&7]=v[i];
    sum+=table[(x>>3)<<shift];
  }
  return sum;
}
__attribute__((noinline,noclone))
unsigned long dependent(unsigned *q, unsigned char *table, unsigned *second,
			unsigned *v, unsigned n)
{
  unsigned long sum=0;
  for(unsigned i=0;i<n;i++) {
    unsigned x=q[i&7]; q[i&7]=v[i];
    sum+=table[second[x]];
  }
  return sum;
}
__attribute__((noinline,noclone))
unsigned long atomic_queue(_Atomic unsigned *q, unsigned char *table,
			   unsigned *v, unsigned n)
{
  unsigned long sum=0;
  for(unsigned i=0;i<n;i++) {
    unsigned x=atomic_load(q+(i&7)); atomic_store(q+(i&7),v[i]);
    sum+=table[x];
  }
  return sum;
}
int main(void)
{
  unsigned char *table=malloc(32768);
  unsigned q[8],v[128],second[256]; _Atomic unsigned aq[8];
  if(!table) abort();
  for(unsigned i=0;i<32768;i++) table[i]=(i*13+11)^((i>>6)*3);
  for(unsigned i=0;i<256;i++) second[i]=(i*7)&255;
  unsigned long sum=0; uint32_t seed=1;
  for(unsigned r=0;r<200;r++) {
    for(unsigned i=0;i<8;i++) q[i]=i,aq[i]=i;
    for(unsigned i=0;i<128;i++) { seed=seed*1664525u+1013904223u; v[i]=(seed>>16)&255; }
    sum+=replacement(q,table,v,128);
    sum+=different(q,table,v,128);
    sum+=volatile_queue(q,table,v,128);
    sum+=variable(q,table,v,128,r%6);
    sum+=dependent(q,table,second,v,128);
    sum+=atomic_queue(aq,table,v,128);
    for(unsigned i=0;i<8;i++) sum+=q[i]+aq[i];
  }
  /* New entries may never be consumed.  The new hint must not fault. */
  for(unsigned i=0;i<8;i++) q[i]=0;
  v[0]=UINT32_MAX;
  sum+=replacement(q,table,v,1);
  sum+=variable(q,table,v,0,UINT32_MAX);
  if (sum != 19526928UL) __builtin_abort();
  free(table);
}
