/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O3 -fconvert-minmax" } */

#include <inttypes.h>

__attribute__((noinline))
void test (int8_t *restrict a, int32_t *restrict x)
{
  for (int i = 0; i < 8; i++)
    a[i] = ((x[i] & ~((1 << 9)-1)) ? (-x[i])>>31 & ((1 << 9)-1) : x[i]);
}

/* { dg-final { scan-assembler-times {smax\t} 4 } }  */
/* { dg-final { scan-assembler-times {smin\t} 4 } }  */
