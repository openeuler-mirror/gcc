/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O3 -fconvert-minmax" } */

#include <inttypes.h>

__attribute__((noinline))
void test (int32_t *restrict a, int32_t *restrict x)
{
  for (int i = 0; i < 4; i++)
    a[i] = ((((-x[i]) >> 31) ^ x[i])
            & (-((int32_t)((x[i] & (~((1 << 8)-1))) == 0)))) ^ ((-x[i]) >> 31);
}

/* { dg-final { scan-assembler-not {smax\t} } }  */
/* { dg-final { scan-assembler-not {smin\t} } }  */
