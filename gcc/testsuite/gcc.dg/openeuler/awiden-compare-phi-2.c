/* Two candidate loops in one function: one loop's live-out value gives
   the other loop's exit block a PHI that is not that loop's induction
   variable.  Same root cause as awiden-compare-phi-1.c (empty PHI
   argument slots after the rewrite, compiler segfault); the affected
   candidate must be rejected.  No scan count is asserted - how many of
   the two loops remain transformable is not the point - the test pins
   that compilation succeeds and the results stay byte-wise correct.  */
/* { dg-do run { target {{ aarch64*-*-linux* } && lp64 } } } */
/* { dg-options "-O3 -farray-widen-compare" } */

#include <stdint.h>

#define my_min(x, y) ((x) < (y) ? (x) : (y))

__attribute__ ((noipa)) uint64_t
two (uint32_t len0, uint32_t len1, const uint32_t lim1, const uint32_t lim2,
     const uint8_t *const pb, const uint8_t *const cur,
     const uint8_t *const pb2, const uint8_t *const cur2)
{
  uint32_t len = my_min (len0, len1);
  while (++len != lim1)
    if (pb[len] != cur[len])
      break;
  uint32_t r1 = len;
  uint32_t ln2 = my_min (len0, len1);
  while (++ln2 != lim2)
    if (pb2[ln2] != cur2[ln2])
      break;
  return (uint64_t) r1 * 1000000u + ln2;
}

int
main (void)
{
  static uint8_t a[128], b[128], c[128], d[128];
  for (uint32_t i = 0; i < 128; i++)
    {
      a[i] = b[i] = (uint8_t) (i * 7 + 3);
      c[i] = d[i] = (uint8_t) (i * 13 + 5);
    }
  b[33] ^= 0x10;
  d[57] ^= 0x20;
  if (two (0, 0, 100, 100, a, b, c, d) != 33u * 1000000u + 57u)
    __builtin_abort ();
  if (two (0, 0, 30, 40, a, b, c, d) != 30u * 1000000u + 40u)
    __builtin_abort ();
  return 0;
}
