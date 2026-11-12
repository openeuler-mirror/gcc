/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -fno-tree-vectorize -fno-prefetch-loop-arrays -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

/* The replacement path must run independently of array prefetching.  */
#include "replacement-index-prefetch-1.c"

/* { dg-final { scan-tree-dump-times "Replacement-index prefetch:" 1 "aprefetch" } } */
