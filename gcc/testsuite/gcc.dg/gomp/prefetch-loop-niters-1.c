/* Do not convert an out-of-range OpenMP iteration count to HOST_WIDE_INT.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fprefetch-loop-arrays -fnon-temporal-store" } */

void
f (double *p)
{
#pragma omp parallel for
  for (unsigned long long i = 0; i < 0xfffffffffffffff0ULL; i++)
    p[i] = (double) i * 3.0 + 1.0 / (i + 1.0);
}
