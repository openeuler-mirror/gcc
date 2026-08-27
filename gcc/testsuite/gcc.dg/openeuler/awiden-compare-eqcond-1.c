/* The else-break form reaches the pass as an EQ_EXPR compare whose true
   edge stays in the loop.  The rebuilt loops reuse the condition code
   with hard-wired exit polarity, so this shape used to be rewritten with
   inverted semantics (exit on equal words, __builtin_ctzll of an all-zero
   XOR - wrong result 12 instead of 20).  It must be rejected (scan 0)
   and computed byte-wise.  */
/* { dg-do run { target {{ aarch64*-*-linux* } && lp64 } } } */
/* { dg-options "-O3 -farray-widen-compare -fdump-tree-awiden_compare-details" } */

#include <stdint.h>

#define my_min(x, y) ((x) < (y) ? (x) : (y))

__attribute__ ((noipa)) uint32_t
func (uint32_t len0, uint32_t len1, const uint32_t len_limit,
      const uint8_t *const pb, const uint8_t *const cur)
{
  uint32_t len = my_min (len0, len1);
  while (++len != len_limit)
    {
      if (pb[len] == cur[len])
	;
      else
	break;
    }
  return len;
}

int
main (void)
{
  static uint8_t a[64], b[64];
  for (uint32_t i = 0; i < 64; i++)
    a[i] = b[i] = (uint8_t) (i * 3);
  b[20] ^= 0x40;
  if (func (0, 0, 40, a, b) != 20)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "loop form is success" 0 "awiden_compare" } } */
