/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */
/* { dg-require-effective-target vect_int } */
#include <stdio.h>
#include <stdlib.h>
#include "tree-vect.h"

#define N 16
#define M 256

int foo (unsigned char *pix1, int i_pix1, unsigned char *pix2, int i_pix2)
{
  int i = 0;
  int sum = 0;
  unsigned char c0[N], c1[N];
  for (int i = 0; i < N/2; i++, pix1 += i_pix1, pix2 += i_pix2)
    {
      c0[i] = pix1[0] - pix2[0];
      c1[i] = pix1[1] - pix2[1];
    }
  for (int i = N/2; i < N; i++, pix1 += i_pix1, pix2 += i_pix2)
    {
      c0[i] = pix1[0] - pix2[0];
      c1[i] = pix1[1] - pix2[1];
   }
  for (int i = 0; i < N; i++)
    {
      sum += c0[i] + c1[i];
    }
  return sum;
}

int main (int argc, const char* argv[])
{
  unsigned char input1[M];
  unsigned char input2[M];
  int i1 = 6;
  int i2 = 4;
  check_vect ();
  for (int i = 0; i < M; i++)
    {
	input1[i] = i * 5;
	input2[i] = i * 2;
    }
  int sum = foo (input1, i1, input2, i2);
  if (sum != 3280)
    {
      abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized using transposed version" "slp1" } } */
