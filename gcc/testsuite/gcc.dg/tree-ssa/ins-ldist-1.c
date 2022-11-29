/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-do run { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -ftree-slp-transpose-vectorize -fdump-tree-ldist-all-details -save-temps" } */

#include <stdio.h>
#include <stdlib.h>

static unsigned inline abs2 (unsigned a)
{
  unsigned s = ((a>>15)&0x10001)*0xffff;
  return (a+s)^s;
}

int foo (unsigned char *oxa, int ia, unsigned char *oxb, int ib)
{
  unsigned tmp[4][4];
  unsigned a0, a1, a2, a3;
  int sum = 0;
  for (int i = 0; i < 4; i++, oxa += ia, oxb += ib)
    {
      a0 = (oxa[0] - oxb[0]) + ((oxa[4] - oxb[4]) << 16);
      a1 = (oxa[1] - oxb[1]) + ((oxa[5] - oxb[5]) << 16);
      a2 = (oxa[2] - oxb[2]) + ((oxa[6] - oxb[6]) << 16);
      a3 = (oxa[3] - oxb[3]) + ((oxa[7] - oxb[7]) << 16);
      int t0 = a0 + a1;
      int t1 = a0 - a1;
      int t2 = a2 + a3;
      int t3 = a2 - a3;
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

int main ()
{
  unsigned char oxa[128] = {0};
  unsigned char oxb[128] = {0};
  for (int i = 0; i < 128; i++)
    {
      oxa[i] += i * 3;
      oxb[i] = i * 2;
    }
  int sum = foo (oxa, 16, oxb, 32);
  if (sum != 736)
    {
      abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "Insertion done: 4 temp arrays inserted" 1 "ldist" } } */
/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 1 "ldist" } } */
