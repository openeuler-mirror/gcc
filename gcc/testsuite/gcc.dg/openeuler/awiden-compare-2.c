/* { dg-do compile { target {{ aarch64*-*-linux* } && lp64 } } } */
/* { dg-options "-O3 -mabi=lp64 -farray-widen-compare -fdump-tree-awiden_compare-details" } */

#include <stdint.h>
#include <stdio.h>

#define EMPTY_HASH_VALUE 0
#define my_min(x, y) ((x) < (y) ? (x) : (y))
#define true 1

typedef struct {
  uint32_t len;
  uint32_t dist;
} lzma_match;


lzma_match *
func (
  const uint32_t len_limit,
  const uint32_t pos,
  const uint8_t *const cur,
  uint32_t cur_match,
  uint32_t depth,
  uint32_t *const son,
  const uint32_t cyclic_pos,
  const uint32_t cyclic_size,
  lzma_match *matches,
  uint32_t len_best)
{
  uint32_t *ptr0 = son + (cyclic_pos << 1) + 1;
  uint32_t *ptr1 = son + (cyclic_pos << 1);

  uint32_t len0 = 0;
  uint32_t len1 = 0;

  while (true)
    {
      const uint32_t delta = pos - cur_match;
      if (depth-- == 0 || delta >= cyclic_size)
        {
          *ptr0 = EMPTY_HASH_VALUE;
          *ptr1 = EMPTY_HASH_VALUE;
          return matches;
        }

      uint32_t *const pair = son + ((cyclic_pos - delta + (delta > cyclic_pos ? cyclic_size : 0)) << 1);

      const uint8_t *const pb = cur -delta;
      uint32_t len = my_min(len0, len1);

      if (pb[len] == cur[len])
        {
          while (++len != len_limit)
            if (pb[len] != cur[len])
              break;

          if (len_best < len)
            {
              len_best = len;
              matches->len = len;
              matches->dist = delta - 1;
              ++matches;

              if (len == len_limit)
                {
                  *ptr1 = pair[0];
                  *ptr0 = pair[1];
                  return matches;
                }
            }
        }

      if (pb[len] < cur[len])
        {
          *ptr1 = cur_match;
          ptr1 = pair + 1;
          cur_match = *ptr1;
          len1 = len;
        }
        else
        {
          *ptr0 = cur_match;
          ptr0 = pair;
          cur_match = *ptr0;
          len0 = len;
        }
    }
}

/* { dg-final { scan-tree-dump-times "loop form is success" 1 "awiden_compare"} } */
