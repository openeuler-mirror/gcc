/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details --param=simultaneous-prefetches=0" } */
#include "replacement-index-edge.h"
/* { dg-final { scan-tree-dump-not "Replacement-edge prefetch:" "aprefetch" } } */
/* { dg-final { scan-assembler-not "prfm" } } */
