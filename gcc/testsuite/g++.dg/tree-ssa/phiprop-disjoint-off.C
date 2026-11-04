/* An explicit disable must override a preceding enable.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -ftree-phiprop-disjoint-stores -fno-tree-phiprop-disjoint-stores -fdump-tree-phiprop2-details" } */

#include "phiprop-disjoint-1.C"

/* { dg-final { scan-tree-dump-not "PHI load propagation: crossed proven-disjoint local stores" "phiprop2" } } */
