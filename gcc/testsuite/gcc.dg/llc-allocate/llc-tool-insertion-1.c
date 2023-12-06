/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -march=armv8.2-a+sve -funroll-loops -ffast-math -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param target-variables=lPtr" } */

#include <stdio.h>

#define N 131590
#define F 384477

double diagPtr[N];
double psiPtr[N];
double ApsiPtr[N];
int lPtr[F];
int uPtr[F];
double lowerPtr[F];
double upperPtr[F];

void
AMUL (double *diagPtr, double *psiPtr, double *ApsiPtr, int *lPtr,
      int *uPtr, double *lowerPtr, double *upperPtr, int nCells, int nFaces)
{
  for (int cell=0; cell<nCells; cell++)
    ApsiPtr[cell] = diagPtr[cell]*psiPtr[cell];

  for (int face=0; face<nFaces; face++)
    {
      ApsiPtr[uPtr[face]] += lowerPtr[face]*psiPtr[lPtr[face]];
      ApsiPtr[lPtr[face]] += upperPtr[face]*psiPtr[uPtr[face]];
    }
}

int
main (int argc, char *argv[])
{
  int nCells = N;
  int nFaces = F;
  int testIter = 2;

  for (int i=0; i<testIter; i++)
    AMUL (diagPtr,psiPtr,ApsiPtr,lPtr,uPtr,lowerPtr,upperPtr,nCells,nFaces);

  return 0;
}

/* { dg-final { scan-tree-dump-times "NOTICE: Prefetching target variable \""
                " lPtr \"" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not "runtime issue" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "static issue" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "insert prfm" 2 "llc_allocate" } } */
