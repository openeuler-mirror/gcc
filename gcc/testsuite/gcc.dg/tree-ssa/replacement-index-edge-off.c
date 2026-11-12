/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details -fno-tree-replacement-index-prefetch" } */
#include "replacement-index-edge.h"
/* { dg-final { scan-assembler-not "prfm" } } */
