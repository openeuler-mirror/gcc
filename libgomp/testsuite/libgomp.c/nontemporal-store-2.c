/* Non-temporal stores to char arrays in an OpenMP loop.  */
/* { dg-do run { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -fnon-temporal-store -fdump-tree-aprefetch-details -save-temps -fopenmp" } */

#include <assert.h>

#define loop_num 1000000
typedef unsigned long long ssize_t;
typedef unsigned char DP;

DP a[loop_num + 10];
DP b[loop_num + 10];
DP c[loop_num + 10];

void __attribute__ ((noinline)) test ()
{
  ssize_t j;
#pragma omp parallel for
  for (j = 0; j < loop_num; j++)
    {
      c[j] = a[j] + b[j];
    }
}

int
main ()
{
  for (ssize_t i = 0; i < loop_num; i++)
    {
      a[i] = (DP) i;
      b[i] = (DP) i;
    }

  test ();

  for (ssize_t i = 0; i < loop_num; i++)
    {
      assert (c[i] == (DP) (a[i] + b[i]));
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "Marked reference .*? as a nontemporal store" 3 "aprefetch1" } } */
/* { dg-final { scan-tree-dump-times "Marked reference .*? as a nontemporal store" 3 "aprefetch2" } } */
/* { dg-final { scan-assembler "movntdq" } } */
