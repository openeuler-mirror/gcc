/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -march=armv8.2-a+sve -funroll-loops -ffast-math -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param force-issue=1 --param filter-mode=0" } */

#include <stdio.h>

#define N 100000

int A_i[N];
int A_j[N];
double A_data[N];
double x_data[N];
double y_data[N];
int num_rows = N;

void
MatMult (int *A_i, int *A_j, double *A_data, double *x_data,
         int num_rows, double *y_data)
{
  int i = 0;
  int j = 0;
  double temp = 0;
  for (i = 0; i < num_rows; i++)
    {
      temp = y_data[i];
      for (j = A_i[i]; j < A_i[i+1]; j++)
        temp += A_data[j] * x_data[A_j[j]];
      y_data[i] = temp;
    }
}

int
main (int argc, char *argv[])
{
  int testIter = 2;

  for (int i = 0; i < testIter; i++)
    MatMult (A_i, A_j, A_data, x_data, num_rows, y_data);

  return 0;
}

/* { dg-final { scan-tree-dump-times "ref_count = (?:\[3-9\]|\[1-9\]\\d{1,}), ninsns = \[1-9\]\\d*, mem_to_insn_ratio = 0.\[2-9\]\\d*" 4 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "Tracing succeeded" 14 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not   "Tracing failed" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not   "static_data_size:" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "\{ (?:\\d+\\(\\d+\\) ){1}\}" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not   ", size: (?!(0\.000000))" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times ", size: 0\.000000" 6 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "\\d\\tx_data\\t\\(0.000000, 1, 1, 0\\)" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "\\d\\tA_j\\t\\(0.000000, 1, 1, 0\\)" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "\\d\\tA_data\\t\\(0.000000, 1, 1, 0\\)" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not   "runtime issue" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "static issue" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "insert svprfd_gather" 2 "llc_allocate" } } */
