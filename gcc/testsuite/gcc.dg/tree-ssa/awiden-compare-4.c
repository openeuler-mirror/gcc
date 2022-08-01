/* { dg-do compile { target {{ aarch64*-*-linux* } && lp64 } } } */
/* { dg-options "-O3 -mabi=lp64 -farray-widen-compare -fdump-tree-awiden_compare-details" } */

#include <stdint.h>
#include <stdio.h>

#define my_min(x, y) ((x) < (y) ? (x) : (y))

uint32_t
func (uint32_t len0, uint32_t len1, const uint32_t len_limit, const uint8_t *const pb, const uint8_t *const cur)
{
   uint32_t len = my_min(len0, len1);
    while (len != len_limit)
    {
        if (pb[len] != cur[len])
            break;
        len = len + 2;
    }
    return len;
}

/* { dg-final { scan-tree-dump-times "loop form is success" 0 "awiden_compare"} } */
