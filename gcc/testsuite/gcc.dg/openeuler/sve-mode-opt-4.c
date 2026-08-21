/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3 -floop-sve-mode-opt -march=armv8-a+sve -fdump-tree-vect-details" } */
#include<stdint.h>

void foo(unsigned int* dest, uint8_t* src, unsigned int len, unsigned int* mul)
{
  for(int i = 0; i < len; ++i)
    dest[i] = src[i] * (*mul);
}

/* { dg-final { scan-tree-dump-not "Loop sve mode optimization success" "vect" } } */