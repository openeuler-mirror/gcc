/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+sve" } */
#include<stdint.h>

void foo(unsigned int* dest, uint8_t* src, unsigned int len, unsigned int* mul)
{
  for(int i = 0; i < len; ++i)
    dest[i] = src[i] * (*mul) + 8;
}

/* { dg-final { scan-tree-dump-times "Loop sve mode optimization success" 1 "vect" } } */