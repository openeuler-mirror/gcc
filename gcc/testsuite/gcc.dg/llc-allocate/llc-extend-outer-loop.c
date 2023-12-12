/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -march=armv8.2-a+sve -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param=outer-loop-nums=10 --param=issue-topn=4 --param=force-issue=1 --param=filter-kernels=0" } */
#include <stdio.h>
#define N 131590
#define F 384477

int ownStartPtr[F];
double bPrimePtr[N];
double diagPtr[N];
double psiPtr[N];
double upperPtr[F];
double lowerPtr[F];
int uPtr[F];

void SMOOTH(int *ownStartPtr, double *bPrimePtr, double *diagPtr, double *psiPtr, int *uPtr, double *lowerPtr, double *upperPtr, int nCells);

int main(int argc, char *argv[])
{
    int nCells = N;
    int nFaces = F;
    int testIter = 2;
    for (int i = 0; i < testIter; i++)
      {
        SMOOTH(ownStartPtr, bPrimePtr, diagPtr, psiPtr, uPtr, lowerPtr, upperPtr, nCells);
      }
  return  0;
}


void SMOOTH(int *ownStartPtr, double *bPrimePtr, double *diagPtr, double *psiPtr, int *uPtr, double *lowerPtr, double *upperPtr, int nCells)
{
    double psii;
	  int fStart;
    int fEnd = ownStartPtr[0];

    for (int celli = 0; celli < nCells; celli++)
      {
        fStart = fEnd;
        fEnd = ownStartPtr[celli + 1];
        psii = bPrimePtr[celli];

        for (int facei = fStart; facei<fEnd; facei++)
          {
            psii -= upperPtr[facei] * psiPtr[uPtr[facei]];
          }

        psii /= diagPtr[celli];
        for (int facei = fStart; facei < fEnd; facei++)
          {
            bPrimePtr[uPtr[facei]] -= lowerPtr[facei] * psii;
          }
        psiPtr[celli] = psii;
      }
}

/* { dg-final { scan-tree-dump-times "bPrimePtr : 3" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "diagPtr : 1" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "upperPtr : 1" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "psiPtr : 2" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "insert" 8 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not   "Processing loop 0"  "llc_allocate" } } */
