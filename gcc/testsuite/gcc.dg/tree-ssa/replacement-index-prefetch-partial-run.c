/* { dg-do run { target aarch64*-*-linux* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2" } */
#include "replacement-index-prefetch-partial.c"
#include "replacement-index-edge-partial.c"
#include <stdlib.h>
#include <string.h>

/* Check demand values and queue updates separately from the compile tests
   that check the actual hint operands.  */
__attribute__((noipa, optimize("O0"))) static unsigned long
reference_loop (unsigned *q, const unsigned char *table,
		const unsigned *future, unsigned n)
{
  unsigned long sum = 0;
  for (unsigned i = 0; i < n; ++i)
    {
      unsigned old = q[i & 7];
      q[i & 7] = future[i];
      sum += table[((old & 255u) << 16) + (old >> 8)];
    }
  return sum;
}

int
main (void)
{
  unsigned char *table = malloc (1u << 25);
  if (!table)
    abort ();
  for (unsigned i = 0; i < (1u << 25); ++i)
    table[i] = (i ^ (i >> 16)) & 255;
  unsigned seed = 1;
  for (unsigned r = 0; r < 32; ++r)
    {
      unsigned q[8], reference[8], future[128];
      for (unsigned i = 0; i < 8; ++i)
	q[i] = reference[i] = (seed += 7919);
      for (unsigned i = 0; i < 128; ++i)
	{
	  seed = seed * 1664525u + 1013904223u;
	  future[i] = seed;
	}
      if (partial_loop (q, table, future, 128)
	  != reference_loop (reference, table, future, 128)
	  || memcmp (q, reference, sizeof q))
	abort ();
      unsigned old = seed, next = ~seed, other = seed >> 3;
      for (unsigned choose = 0; choose < 2; ++choose)
	{
	  unsigned entry = old, v = choose ? old : other;
	  unsigned long expected
	    = table[((v & 255u) << 16) + (v >> 8)];
	  for (unsigned i = 0; i < r; ++i)
	    expected += table[i & 15];
	  if (partial_edge (&entry, table, next, other, choose, r) != expected
	      || entry != (choose ? next : old))
	    abort ();
	}
    }
  unsigned q[8] = { 0 }, future[1] = { 1 };
  if (partial_loop (q, table, future, 1) != table[0] || q[0] != 1)
    abort ();
  if (partial_edge (q, table, 0, 0, 1, 0) != table[65536] || q[0] != 0)
    abort ();
  free (table);
  return 0;
}
