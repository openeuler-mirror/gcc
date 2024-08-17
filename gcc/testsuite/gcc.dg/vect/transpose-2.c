/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-additional-options "-fno-tree-loop-vectorize -fno-tree-dse" } */
/* { dg-require-effective-target vect_int } */
#include <stdio.h>
#include <stdlib.h>
#include "tree-vect.h"

#define N 8
#define M 256

int foo (unsigned char *pix1, int i_pix1, unsigned char *pix2, int i_pix2)
{
  int i = 0;
  int sum = 0;
  unsigned short c0[N], c1[N], c2[N], c3[N], c4[N], c5[N], c6[N], c7[N];
  for (i = 0; i < N; i++, pix1 += i_pix1, pix2 += i_pix2)
    {
      c0[i] = pix1[0] - pix2[0];
      c1[i] = pix1[1] - pix2[1];
      c2[i] = pix1[2] - pix2[2];
      c3[i] = pix1[3] - pix2[3];
    }
  for (int i = 0; i < N; i++)
    {
      sum += c0[i] + c1[i] + c2[i] + c3[i];
    }
  return sum;
}

int main (int argc, const char* argv[])
{
  unsigned char input1[M];
  unsigned char input2[M];
  int i1 = 5;
  int i2 = 4;
  check_vect ();
  for (int i = 0; i < M; i++)
    {
	input1[i] = i * 4;
	input2[i] = i * 2;
    }
  int sum = foo (input1, i1, input2, i2);
  if (sum != 1440)
    {
      abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized using transposed version" "slp1" } } */
