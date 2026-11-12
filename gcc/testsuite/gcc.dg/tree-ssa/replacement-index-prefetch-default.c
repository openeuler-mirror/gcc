/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize" } */

#include "replacement-index-prefetch-1.c"

/* { dg-final { scan-assembler-not "prfm" } } */
