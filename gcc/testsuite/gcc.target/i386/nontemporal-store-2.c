/* Scalar non-temporal stores with vectorization disabled.  */
/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -fnon-temporal-store -fdump-tree-aprefetch-details -save-temps -fno-tree-vectorize" } */

#include <assert.h>

#define loop_num 100000
typedef unsigned long long ssize_t;
typedef int DP;
typedef long DP2;
typedef long long DP3;

DP a[loop_num + 10];
DP b[loop_num + 10];

DP2 d[loop_num + 10];
DP2 e[loop_num + 10];

DP3 q[loop_num + 10];
DP3 w[loop_num + 10];

int
main ()
{
  for (ssize_t i = 0; i < loop_num; i++)
    {
      a[i] = (DP) i;
      b[i] = (DP) i;
    }
  for (ssize_t i = 0; i < loop_num; i++)
    {
      d[i] = (DP2) i;
      e[i] = (DP2) i;
    }
  for (ssize_t i = 0; i < loop_num; i++)
    {
      q[i] = (DP3) i;
      w[i] = (DP3) i;
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "Marked reference .*? as a nontemporal store" 6 "aprefetch2" } } */
/* { dg-final { scan-assembler-not "movntdq" } } */
/* { dg-final { scan-assembler "movnti" } } */
