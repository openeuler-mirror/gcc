/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize --param=vect-swap-operands=1 -fdump-tree-slp-details" } */

void foo (float *res, float a, float b, float c)
{
  res[0] = a * b;
  res[1] = b * c;
}

/* { dg-final { scan-tree-dump "Swapped operands for" "slp2" } } */
