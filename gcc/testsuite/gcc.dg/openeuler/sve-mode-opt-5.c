/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3 -floop-sve-mode-opt -march=armv8-a+sve -fdump-tree-vect-details" } */
#include<stdint.h>

void foo(unsigned int* dest, uint8_t* src, unsigned int len, 
    unsigned int* mul, unsigned int* append)
{
  for(int i = 0; i < len; ++i)
    dest[i] = ((unsigned int)src[i]) * (*mul) + (*append);
}

/* { dg-final { scan-tree-dump-times "Loop sve mode optimization success" 1 "vect" } } */