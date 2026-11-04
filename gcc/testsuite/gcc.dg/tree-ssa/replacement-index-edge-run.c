/* { dg-do run { target aarch64*-*-linux* } } */
/* { dg-options "-O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */
#define _GNU_SOURCE
#include "replacement-index-edge.h"
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/mman.h>
#include <unistd.h>
__attribute__((noipa,optimize("O0"))) static unsigned
ref_store (unsigned *q, unsigned *t, const unsigned *s, unsigned next,
	   unsigned other, unsigned choose, unsigned n)
{
  unsigned v;
  if (choose) { v = *q; *q = next; } else v = other;
  unsigned x = s[v & 255];
  t[(v >> 8) & 255] = v;
  unsigned tail = 0;
  for (unsigned i = 0; i < n; ++i) tail += t[i & 15];
  return x + tail;
}
int main (void)
{
  unsigned t[256], r[256], s[256];
  uint32_t seed = 1;
  for (unsigned i = 0; i < 1024; ++i)
    {
      for (unsigned j = 0; j < 256; ++j)
	{ t[j] = j * 13 + i; s[j] = j * 17; }
      seed = seed * 1664525u + 1013904223u;
      unsigned choose = i & 1, n = i & 31, other = seed;
      unsigned q = seed, next = i & 2 ? UINT_MAX : 0, tail = 0;
      for (unsigned j = 0; j < n; ++j) tail += t[j & 15];
      unsigned expected = t[(choose ? q : other) & 255] + tail;
      if (edge_read32 (choose ? &q : 0,t,next,other,choose,n) != expected
	  || q != (choose ? next : seed)) abort ();
      uint64_t q64 = (uint64_t)seed << 32 | seed;
      uint64_t before = q64, other64 = ~q64;
      uint64_t expected64 = t[(choose ? q64 : other64) & 255]
			   + (uint64_t)tail;
      if (edge_read64 (choose ? &q64 : 0,t,UINT64_MAX,other64,choose,n)
	  != expected64 || q64 != (choose ? UINT64_MAX : before)) abort ();
      memcpy (r,t,sizeof t); q = seed; unsigned qr = q;
      if (edge_store (choose ? &q : 0,t,s,next,other,choose,n)
	  != ref_store (choose ? &qr : 0,r,s,next,other,choose,n)
	  || q != qr || memcmp (t,r,sizeof t)) abort ();
      /* Queue aliases the destination: demand store order must survive.  */
      memcpy (t,s,sizeof t); memcpy (r,t,sizeof t);
      if (edge_store (t+1,t,s,UINT_MAX,other,1,n)
	  != ref_store (r+1,r,s,UINT_MAX,other,1,n)
	  || memcmp (t,r,sizeof t)) abort ();
      /* The read base may alias the queue, too.  */
      memcpy (t,s,sizeof t); t[0] = 0;
      if (edge_read32 (t,t,UINT_MAX,0,1,0) != UINT_MAX) abort ();
    }
  long page = sysconf (_SC_PAGESIZE);
  if (page < 4096) abort ();
  unsigned char *m = mmap (0,page*3,PROT_NONE,
			  MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
  if (m == MAP_FAILED || mprotect (m+page,page,PROT_READ|PROT_WRITE))
    abort ();
  unsigned *end = (unsigned *)(m+page*2) - 256;
  for (unsigned j = 0; j < 256; ++j) end[j] = j;
  unsigned q = 255;
  if (edge_read32 (&q,end,UINT_MAX,0,1,0) != 255 || q != UINT_MAX)
    abort ();
  /* The bypass must not touch even an inaccessible queue pointer.  */
  if (edge_read32 ((unsigned *)m,end,UINT_MAX,255,0,0) != 255)
    abort ();
  munmap (m,page*3);
  return 0;
}
/* { dg-final { scan-tree-dump-times "Replacement-edge prefetch:" 3 "aprefetch" } } */
