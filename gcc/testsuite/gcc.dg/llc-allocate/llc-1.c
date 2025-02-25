/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -march=armv8.2-a+sve -funroll-loops -ffast-math -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param issue-topn=2 --param branch-prob-threshold=50 --param filter-mode=0" } */

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

/* { dg-final { scan-tree-dump-times "ref_count = (?:\[3-9\]|\[1-9\]\\d{1,}), ninsns = \[1-9\]\\d*, mem_to_insn_ratio = 0.\[2-9\]\\d*" 5 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "Tracing succeeded" 29 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not   "Tracing failed" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "static_data_size:" 7 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "\{ (?:\\d+\\(\\d+\\) ){2}\}" 3 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "\{ (?:\\d+\\(\\d+\\) ){3}\}" 1 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times ", size: (?!(0\.000000))" 7 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times ", size: 0\.000000" 19 "llc_allocate" } } */
/* { dg-final { scan-tree-dump       "\\d\\tApsiPtr\\t\\(1.003952, 1, 5, 0\\)" "llc_allocate" } } */
/* { dg-final { scan-tree-dump       "\\d\\tpsiPtr\\t\\(1.003952, 1, 3, 0\\)" "llc_allocate" } } */
/* { dg-final { scan-tree-dump       "\\d\\tdiagPtr\\t\\(1.003952, 1, 1, 0\\)" "llc_allocate" } } */
/* { dg-final { scan-tree-dump       "\\d\\tlowerPtr\\t\\(2.933319, 1, 1, 0\\)" "llc_allocate" } } */
/* { dg-final { scan-tree-dump       "\\d\\tupperPtr\\t\\(2.933319, 1, 1, 0\\)" "llc_allocate" } } */
/* { dg-final { scan-tree-dump       "\\d\\tlPtr\\t\\(1.466660, 1, 1, 0\\)" "llc_allocate" } } */
/* { dg-final { scan-tree-dump       "\\d\\tuPtr\\t\\(1.466660, 1, 1, 0\\)" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "runtime issue" 1 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "static issue" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "insert svprfd" 4 "llc_allocate" } } */
