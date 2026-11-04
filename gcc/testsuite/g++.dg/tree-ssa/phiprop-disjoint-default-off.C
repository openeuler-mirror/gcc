/* The default policy must not cross stores using the new relaxation.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -fdump-tree-phiprop2-details" } */

#include "phiprop-disjoint-1.C"

/* { dg-final { scan-tree-dump-not "PHI load propagation: crossed proven-disjoint local stores" "phiprop2" } } */
