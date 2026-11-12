/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize --param=vect-register-size-check=1 -fdump-tree-slp-details" } */

extern float arr[256][256][1024];

void foo (int i, int j, float a, float b)
{
  arr[i][j][0] += a;
  arr[i][j][1] += b;
  arr[i][j+1][0] += a;
  arr[i][j+1][1] += b;
  arr[i+1][j][0] += a;
  arr[i+1][j][1] += b;
  arr[i+1][j+1][0] += a;
  arr[i+1][j+1][1] += b;
}

/* { dg-final { scan-tree-dump "Basic block will be vectorized using SLP" "slp2" } } */
