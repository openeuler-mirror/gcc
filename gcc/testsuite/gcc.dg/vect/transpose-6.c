/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "tree-vect.h"

#define N 4
#define M 256
#define eps 1e-8

float foo (unsigned char *pix1, int i_pix1, unsigned char *pix2, int i_pix2)
{
  unsigned a0[N];
  unsigned a1[N];
  unsigned a2[N];
  unsigned a3[N];

  float c0[N];
  float c1[N];
  float c2[N];
  float c3[N];

  for (int i = 0; i < N; i++, pix1 += i_pix1, pix2 += i_pix2)
    {
      a0[i] = (pix1[0] - pix2[0]) + ((pix1[4] - pix2[4]) << 16);
      a1[i] = (pix1[1] - pix2[1]) + ((pix1[5] - pix2[5]) << 16);
      a2[i] = (pix1[2] - pix2[2]) + ((pix1[6] - pix2[6]) << 16);
      a3[i] = (pix1[3] - pix2[3]) + ((pix1[7] - pix2[7]) << 16);

      c0[i] = (pix1[0] * pix2[0]) + (pix1[4] * pix2[4]);
      c1[i] = (pix1[1] * pix2[1]) + (pix1[5] * pix2[5]);
      c2[i] = (pix1[2] * pix2[2]) + (pix1[6] * pix2[6]);
      c3[i] = (pix1[3] * pix2[3]) + (pix1[7] * pix2[7]);
    }

  float sum = 0;
  for (int i = 0; i < N; i++)
    {
      sum += a0[i] + a1[i] + a2[i] + a3[i] + c0[i] + c1[i] + c2[i] + c3[i];
    }
  return sum;
}

int main (int argc, const char* argv[])
{
  unsigned char input1[M];
  unsigned char input2[M];
  int i1 = 18;
  int i2 = 6;
  check_vect ();
  for (int i = 0; i < M; i++)
    {
	input1[i] = i * 4;
	input2[i] = i * 2;
    }
  float sum = foo (input1, i1, input2, i2);
  if (fabs (sum - 106041168) > eps) 
    {
      abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized using transposed version" "slp1" } } */
/* { dg-final { scan-tree-dump-times "vectorizable_store for slp transpose" 2 "slp1" } } */
