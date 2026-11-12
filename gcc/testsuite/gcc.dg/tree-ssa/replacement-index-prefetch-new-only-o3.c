/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-options "-O3 -fno-tree-vectorize -fno-prefetch-loop-arrays -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

#include "replacement-index-prefetch-1.c"

/* { dg-final { scan-tree-dump-times "Replacement-index prefetch:" 1 "aprefetch" } } */
