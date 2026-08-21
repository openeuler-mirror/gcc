/* Runtime equivalence of the widened byte-compare loop: for every
   mismatch position the rewritten loop must return the same result as
   the original.  The dump scan proves the loop really was rewritten;
   without it a recognition regression would turn this test vacuous.  */
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
    if (pb[len] != cur[len])
      break;
  return len;
}

int
main (void)
{
  static uint8_t a[512], b[512];
  for (uint32_t pos = 1; pos < 500; pos++)
    {
      for (uint32_t i = 0; i < 512; i++)
	a[i] = b[i] = (uint8_t) (i * 7);
      b[pos] ^= 0x40;
      if (func (0, 0, 500, a, b) != pos)
	__builtin_abort ();
    }
  /* No mismatch: the loop must run to len_limit.  */
  for (uint32_t i = 0; i < 512; i++)
    a[i] = b[i] = (uint8_t) (i * 7);
  if (func (0, 0, 500, a, b) != 500)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "loop form is success" 1 "awiden_compare" } } */
