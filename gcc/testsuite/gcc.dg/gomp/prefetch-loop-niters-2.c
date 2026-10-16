/* Do not read a builtin function code from an ordinary function declaration.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fprefetch-loop-arrays -fnon-temporal-store" } */

extern int my_nthreads (void);
double a[100000000];

void
g (void)
{
#pragma omp parallel
  {
    int n = 100000000 / my_nthreads ();
    for (int i = 0; i < n; i++)
      a[i] = (double) i * 3.0 + 1.0 / (i + 1.0);
  }
}
