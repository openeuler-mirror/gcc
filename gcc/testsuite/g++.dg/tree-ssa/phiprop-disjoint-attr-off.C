/* Function-specific options must disable the extension as well.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores -fchecking=2 -fdump-tree-phiprop1-details" } */

#define PHIPROP_ATTR __attribute__((optimize("no-tree-phiprop-disjoint-stores")))
#include "phiprop-disjoint-local.C"

/* { dg-final { scan-tree-dump-not "PHI load propagation: crossed proven-disjoint local stores" "phiprop1" } } */
