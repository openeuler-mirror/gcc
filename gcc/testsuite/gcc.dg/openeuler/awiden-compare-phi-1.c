/* A flag set on break lives past the loop, so the shared exit block has
   a PHI that is not the induction variable.  The rewrite can only fill
   exit-block PHI arguments for the induction variable; this shape used
   to be transformed anyway, leaving the flag PHI with empty argument
   slots - malformed gimple, compiler segfault in a later pass.  It must
   be rejected (scan 0) and compile and run correctly.  */
/* { dg-do run { target {{ aarch64*-*-linux* } && lp64 } } } */
/* { dg-options "-O3 -farray-widen-compare -fdump-tree-awiden_compare-details" } */

#include <stdint.h>

#define my_min(x, y) ((x) < (y) ? (x) : (y))

__attribute__ ((noipa)) uint32_t
func (uint32_t len0, uint32_t len1, const uint32_t len_limit,
      const uint8_t *const pb, const uint8_t *const cur)
{
  uint32_t len = my_min (len0, len1);
  uint32_t flag = 0;
  while (++len != len_limit)
    if (pb[len] != cur[len])
      {
	flag = 1;
	break;
      }
  return len + flag;
}

int
main (void)
{
  static uint8_t a[64], b[64];
  for (uint32_t i = 0; i < 64; i++)
    a[i] = b[i] = (uint8_t) (i * 3);
  b[20] ^= 0x40;
  if (func (0, 0, 40, a, b) != 21)
    __builtin_abort ();
  b[20] = a[20];
  if (func (0, 0, 40, a, b) != 40)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "loop form is success" 0 "awiden_compare" } } */
