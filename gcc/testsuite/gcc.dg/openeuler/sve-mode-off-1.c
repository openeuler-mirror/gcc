/* Control for the -floop-sve-mode-opt gate: this is the eligible shape
   of vect/sve-mode-opt-1.c, but compiled WITHOUT the option (the vect
   harness force-adds it for every sve-mode-opt* file, so no test there
   can pin the default-off state).  The optimization must not trigger.  */
/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3 -march=armv8-a+sve -fdump-tree-vect-details" } */

#include <stdint.h>

void
foo (unsigned int *dest, uint8_t *src, unsigned int len, unsigned int *mul)
{
  for (int i = 0; i < len; ++i)
    dest[i] = src[i] * (*mul) + 8;
}

/* { dg-final { scan-tree-dump-not "Loop sve mode optimization success" "vect" } } */
