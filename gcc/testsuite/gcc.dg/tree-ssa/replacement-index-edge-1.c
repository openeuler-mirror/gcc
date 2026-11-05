/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */
#include "replacement-index-edge.h"
/* { dg-final { scan-tree-dump-times "Replacement-edge prefetch:" 3 "aprefetch" } } */
/* { dg-final { scan-assembler-times "prfm\\tPLDL1KEEP" 3 } } */
/* { dg-final { scan-assembler-not "PSTL1KEEP" } } */
