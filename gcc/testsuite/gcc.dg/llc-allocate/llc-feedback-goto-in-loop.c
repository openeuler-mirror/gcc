/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -fllc-allocate -fdump-tree-llc_allocate-details-lineno -c" } */

/* In this deja test case, we test how Phase 4 of llc-allocate pass deals with
   loop that contains a goto statement (which introduces multiple exits for a
   loop). Currently, loops with multiple exits are filtered by Phase 1.  */
#include <stdio.h>

#define N 131

double diagPtr[N];
int psiPtr[N];
double ApsiPtr[N];

void
goto_in_loop (double *diagPtr, int *psiPtr, double *ApsiPtr, int nCells)
{
  for (int cell=0; cell<nCells; cell++)
    {
      if (psiPtr[cell] % 3 == 0)
          goto zero;
      else if (psiPtr[cell] % 3 == 1)
          goto one;
      else
          ApsiPtr[cell] = diagPtr[cell]*psiPtr[cell];
    }

  zero:
      ApsiPtr[0] = 0.;
      return;
  one:
      ApsiPtr[0] = 1.;
}

int
main (int argc, char *argv[])
{
  int nCells = N;
  int testIter = 2;

  for (int i=0; i<testIter; i++)
    {
      goto_in_loop (diagPtr,psiPtr,ApsiPtr,nCells);
    }
  return  0;
}

/* { dg-final { scan-tree-dump "loop_multiple_exits" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not "Phase 2" "llc_allocate" } } */
/* { dg-final { scan-tree-dump-not "static issue" "llc_allocate" } } */
