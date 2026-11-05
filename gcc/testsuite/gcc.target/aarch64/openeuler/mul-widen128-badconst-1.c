/* { dg-do run } */
/* { dg-options "-O2 -mmul-widen128" } */

/* The same statement shapes over a 16/16 split are NOT the low half of
   x*y, and must not be rewritten as if they were.  This is the hole that
   made the previous match.pd implementation miscompile.  */

typedef unsigned long long uint64_t;

static uint64_t __attribute__((noipa))
split16 (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xffffULL, xh = x >> 16;
  uint64_t yl = y & 0xffffULL, yh = y >> 16;
  return xl * yl + ((xl * yh + xh * yl) << 16);
}

static uint64_t __attribute__((noipa))
reference (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xffffULL, xh = x >> 16;
  uint64_t yl = y & 0xffffULL, yh = y >> 16;
  uint64_t a = xl * yl, b = xl * yh, c = xh * yl;
  uint64_t s = b + c;
  __asm__ volatile ("" : "+r" (a), "+r" (s));
  return a + (s << 16);
}

int
main (void)
{
  uint64_t x = 0x123456789abcdefULL, y = 0xfedcba987654321ULL;
  if (split16 (x, y) != reference (x, y))
    __builtin_abort ();
  if (split16 (x, y) == x * y)
    __builtin_abort ();		/* it must not have become x*y */
  return 0;
}
