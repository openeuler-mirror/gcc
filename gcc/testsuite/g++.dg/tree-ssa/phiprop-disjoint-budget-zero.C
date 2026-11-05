/* A zero budget disables only the additional cross-store analysis.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores --param=phiprop-disjoint-stores-max-work=0 -fchecking=2 -fdump-tree-phiprop1-details" } */

#include "phiprop-disjoint-local.C"

int ordinary (int x, int y, bool choose)
{
  int *p = choose ? &x : &y;
  return *p;
}

/* { dg-final { scan-tree-dump-not "PHI load propagation: crossed proven-disjoint local stores" "phiprop1" } } */
/* { dg-final { scan-tree-dump "Inserting PHI for result of load" "phiprop1" } } */
/* { dg-final { scan-tree-dump-times "PHI load propagation: disjoint-store work budget exhausted" 1 "phiprop1" } } */
