/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param filter-kernels=0" } */

#include <stdio.h>
#include <stdlib.h>

#define N 1000

long a[N] = {0};
long b[N] = {0};
long c[N] = {0};

double
referenceTrace (double *psiPtr, int *lPtr, int *uPtr, int nCells)
{
  double sum;
  for (int cell = 0; cell < nCells; cell++)
    {
      // Multi-layer pointer
      sum += psiPtr[lPtr[cell]];
      psiPtr[uPtr[cell]] = sum;

      // Outer pointer, inner array
      sum += psiPtr[b[cell]];
      psiPtr[a[cell]] = sum;

      // Multi-layer array
      sum += a[b[cell]];
      c[a[cell]] = sum;

      // Outer array, inner pointer
      sum += a[lPtr[cell]];
      c[lPtr[cell]] = sum;
    }
  return sum;
}

int
main (int argc, char *argv[])
{
  int testIter = 2;

  double *psiPtr = NULL;
  int *lPtr = NULL;
  int *uPtr = NULL;
  psiPtr = (double *) calloc (N, sizeof(double));
  lPtr = (int *) calloc (N, sizeof(int));
  uPtr = (int *) calloc (N, sizeof(int));

  for (int i = 0; i < testIter; i++)
    referenceTrace (psiPtr, lPtr, uPtr, N);

  free (psiPtr);
  free (lPtr);
  free (uPtr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "Tracing succeeded" 24 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not "Tracing failed" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "unhandled issue scene" 0 "llc_allocate" } } */