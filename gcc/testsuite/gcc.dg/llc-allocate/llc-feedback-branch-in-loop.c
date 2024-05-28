/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -fllc-allocate -fdump-tree-llc_allocate-details-lineno -c" } */

/* In this deja test case, we test how Phase 4 of llc-allocate pass deals with
   loop that contains a branching.  */
#include <stdio.h>

#define N 131590

double diagPtr[N];
double psiPtr[N];
double ApsiPtr[N];

void
branch_in_loop (double *diagPtr, double *psiPtr, double *ApsiPtr, int nCells)
{
  for (int cell=0; cell<nCells; cell++)
    {
      if (psiPtr[cell] > 0)
          ApsiPtr[cell] = 0;
      else
          ApsiPtr[cell] = diagPtr[cell]*psiPtr[cell];
    }
}

int
main (int argc, char *argv[])
{
  int nCells = N;
  int testIter = 100;

  for (int i=0; i<testIter; i++)
    {
      branch_in_loop (diagPtr,psiPtr,ApsiPtr,nCells);
    }
  return  0;
}

/* { dg-final { scan-tree-dump "static issue" "llc_allocate" } } */
