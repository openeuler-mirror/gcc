/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details -funsafe-math-optimizations -fno-tree-reassoc -ftree-vect-analyze-slp-group" } */
void f(double *a, double *res, double m) {
  double res1, res0;
  res1 = 0;
  res0 = 0;
  for (int i = 0; i < 1000; i+=8) {
    res0 += a[i] * m;
    res1 += a[i+1] * m;
    res0 += a[i+2] * m;
    res1 += a[i+3] * m;
    res0 += a[i+4] * m;
    res1 += a[i+5] * m;
    res0 += a[i+6] * m;
    res1 += a[i+7] * m;
  }
  res[0] += res0;
  res[1] += res1;
}
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
