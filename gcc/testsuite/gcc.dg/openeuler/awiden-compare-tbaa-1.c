/* The widened loads used to carry a 'long unsigned int' access type, so
   with strict aliasing FRE could value-number them across an ordered
   uint32_t store and fold the whole compare away (wrong result 40).
   They now use a ref-all, alias-set-0 access path.  The dump scan pins
   the rewrite happening in func; func is then forced inline, so the
   widened loads and the uint32_t store always end up in one function
   for the execution check, independent of the inliner's cost model.  */
/* { dg-do run { target {{ aarch64*-*-linux* } && lp64 } } } */
/* { dg-options "-O3 -farray-widen-compare -fdump-tree-awiden_compare-details" } */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define my_min(x, y) ((x) < (y) ? (x) : (y))

static inline __attribute__ ((always_inline)) uint32_t
func (uint32_t len0, uint32_t len1, const uint32_t len_limit,
      const uint8_t *const pb, const uint8_t *const cur)
{
  uint32_t len = my_min (len0, len1);
  while (++len != len_limit)
    if (pb[len] != cur[len])
      break;
  return len;
}

__attribute__ ((noipa)) uint32_t
driver (uint8_t *a, uint8_t *b, uint32_t v)
{
  /* Malloc'd memory: this store legally sets the effective type, and the
     byte reads of the compare loop may always read it.  */
  *(uint32_t *) (b + 20) = v;
  return func (0, 0, 40, a, b);
}

int
main (void)
{
  uint8_t *a = (uint8_t *) malloc (48);
  uint8_t *b = (uint8_t *) malloc (48);
  if (!a || !b)
    return 0;
  memset (a, 0, 48);
  memset (b, 0, 48);
  if (driver (a, b, 0x01010101) != 20)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "loop form is success" 1 "awiden_compare" } } */
