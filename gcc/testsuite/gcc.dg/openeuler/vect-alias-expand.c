/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize --param=addr-expand-for-alias-check=1 -fdump-tree-slp-details" } */

extern float arr[2][2];

void foo (int i, int j, float a, float b)
{
  arr[i][j] *= a;
  arr[i][j+1] *= b;
}

/* { dg-final { scan-tree-dump "Basic block will be vectorized using SLP" "slp2" } } */
