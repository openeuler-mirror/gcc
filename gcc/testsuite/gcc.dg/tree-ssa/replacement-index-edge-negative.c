/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */
#include <stdatomic.h>
#define ATTR __attribute__((noipa))
extern void barrier(void);
/* OLD survives through a different PHI, but the address PHI takes OTHER
   on the replacement edge.  Do not substitute OLD into every join. */
ATTR unsigned wrong_phi_input(unsigned *q, const unsigned *x,
			      const unsigned *t, unsigned next,
			      unsigned other, unsigned choose)
{
  unsigned old,index;
  if(choose){old=*q;*q=next;index=other;}
  else{old=other;index=*x;}
  unsigned tail=0;
  for(unsigned k=0;k<other;k++)tail+=t[k&15];
  return t[index&255]+old+tail;
}
ATTR unsigned wrong_input(unsigned *q, const unsigned *t, unsigned next,
			   unsigned other, unsigned choose)
{
  unsigned v;
  if (choose) { unsigned old=*q; *q=next; v=other; (void)old; }
  else v=next;
  unsigned tail=0;
  for(unsigned k=0;k<other;k++) tail+=t[k&15];
  return t[v&255]+tail;
}
ATTR unsigned multi_successor(unsigned *q, const unsigned *t, unsigned next,
			      unsigned other, unsigned c, unsigned d)
{
  unsigned v;
  if (c) { v=*q; *q=next; if (d) barrier(); }
  else v=other;
  unsigned tail=0;
  for(unsigned k=0;k<other;k++) tail+=t[k&15];
  return t[v&255]+tail;
}
ATTR unsigned intervening_call(unsigned *q, const unsigned *t, unsigned next,
			       unsigned other, unsigned choose)
{
  unsigned v;
  if (choose) {v=*q; barrier(); *q=next;} else v=other;
  unsigned tail=0;
  for(unsigned k=0;k<other;k++) tail+=t[k&15];
  return t[v&255]+tail;
}
ATTR unsigned intervening_asm(unsigned *q, const unsigned *t, unsigned next,
			      unsigned other, unsigned choose)
{
  unsigned v;
  if (choose) {v=*q; __asm__ __volatile__("" ::: "memory"); *q=next;}
  else v=other;
  unsigned tail=0;
  for(unsigned k=0;k<other;k++) tail+=t[k&15];
  return t[v&255]+tail;
}
ATTR unsigned intervening_write(unsigned *q, unsigned *x, const unsigned *t,
				unsigned next, unsigned other, unsigned choose)
{
  unsigned v;
  if (choose) {v=*q; *x=7; *q=next;} else v=other;
  unsigned tail=0;
  for(unsigned k=0;k<other;k++) tail+=t[k&15];
  return t[v&255]+tail;
}
ATTR unsigned volatile_queue(volatile unsigned *q, const unsigned *t,
			     unsigned next, unsigned other, unsigned choose)
{
  unsigned v;
  if (choose) {v=*q; *q=next;} else v=other;
  unsigned tail=0;
  for(unsigned k=0;k<other;k++) tail+=t[k&15];
  return t[v&255]+tail;
}
ATTR unsigned atomic_queue(_Atomic unsigned *q, const unsigned *t,
			   unsigned next, unsigned other, unsigned choose)
{
  unsigned v;
  if (choose) {v=atomic_load(q); atomic_store(q,next);} else v=other;
  unsigned tail=0;
  for(unsigned k=0;k<other;k++) tail+=t[k&15];
  return t[v&255]+tail;
}
ATTR unsigned signed_queue(int *q, const unsigned *t, int next,
			   int other, unsigned choose)
{
  int v;
  if (choose) {v=*q; *q=next;} else v=other;
  unsigned tail = 0;
  for (unsigned k = 0; k < other; ++k) tail += t[k & 15];
  return t[(unsigned)v&255] + tail;
}

__attribute__((noipa)) unsigned
wide_queue (unsigned __int128 *q, const unsigned *t,
	    unsigned __int128 next, unsigned __int128 other,
	    unsigned choose, unsigned n)
{
  unsigned __int128 v;
  if (choose) { v = *q; *q = next; } else v = other;
  unsigned tail = 0;
  for (unsigned i = 0; i < n; ++i) tail += t[i & 15];
  return t[(unsigned)v & 255] + tail;
}
/* { dg-final { scan-tree-dump-not "Replacement-edge prefetch:" "aprefetch" } } */
/* { dg-final { scan-assembler-not "prfm" } } */
