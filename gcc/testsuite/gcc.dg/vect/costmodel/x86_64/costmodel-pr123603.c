/* { dg-do compile } */
/* This test relies on the EulerGCC-specific c86-4g-m7 cost model.  */
/* { dg-additional-options "-mtune=c86-4g-m7" } */

void foo (int *block)
{
  for (int i = 0; i < 3; ++i)
    {
      int a = block[i * 9];
      int b = block[i * 9 + 1];
      block[i * 9] = a + 10;
      block[i * 9 + 1] = b + 10;
    }
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized using 8 byte vectors" "vect" } } */
