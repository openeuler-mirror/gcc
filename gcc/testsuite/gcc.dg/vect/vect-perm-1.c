/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -fdump-tree-vect-all-details -save-temps" } */

#include <stdio.h>
#include <stdlib.h>

static unsigned inline abs2 (unsigned a)
{
  unsigned s = ((a>>15)&0x10001)*0xffff;
  return (a+s)^s;
}

int foo (unsigned *a00, unsigned *a11, unsigned *a22, unsigned *a33)
{
  unsigned tmp[4][4];
  unsigned a0, a1, a2, a3;
  int sum = 0;
  for (int i = 0; i < 4; i++)
    {
      int t0 = a00[i] + a11[i];
      int t1 = a00[i] - a11[i];
      int t2 = a22[i] + a33[i];
      int t3 = a22[i] - a33[i];
      tmp[i][0] = t0 + t2;
      tmp[i][2] = t0 - t2;
      tmp[i][1] = t1 + t3;
      tmp[i][3] = t1 - t3;
    }
  for (int i = 0; i < 4; i++)
    {
      int t0 = tmp[0][i] + tmp[1][i];
      int t1 = tmp[0][i] - tmp[1][i];
      int t2 = tmp[2][i] + tmp[3][i];
      int t3 = tmp[2][i] - tmp[3][i];
      a0 = t0 + t2;
      a2 = t0 - t2;
      a1 = t1 + t3;
      a3 = t1 - t3;
      sum += abs2 (a0) + abs2 (a1) + abs2 (a2) + abs2 (a3);
    }
  return (((unsigned short) sum) + ((unsigned) sum >>16)) >> 1;
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 16 "vect" } } */
