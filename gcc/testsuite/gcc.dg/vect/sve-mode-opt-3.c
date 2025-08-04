/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+sve" } */
#include<stdint.h>

void foo(unsigned int* dest, uint8_t* src, unsigned int len)
{
  for(int i = 0; i < len; ++i)
    dest[i] = src[i] + 8;
}

/* { dg-final { scan-tree-dump-not "Loop sve mode optimization success" "vect" } } */