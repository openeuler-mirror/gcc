/* Debug statements must not change whether the work budget is exhausted.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores --param=phiprop-disjoint-stores-max-work=5 -fcompare-debug" } */

#include "phiprop-disjoint-local.C"
