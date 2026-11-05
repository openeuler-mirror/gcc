/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-options "-O3 -fno-tree-vectorize -fno-prefetch-loop-arrays -ftree-replacement-index-prefetch --param=l1-cache-line-size=96 -fchecking=2 -fdump-tree-aprefetch-details" } */

/* The new path does not use the legacy array-prefetch cache-line model.  */
#include "replacement-index-prefetch-1.c"

/* { dg-final { scan-tree-dump-times "Replacement-index prefetch:" 1 "aprefetch" } } */
