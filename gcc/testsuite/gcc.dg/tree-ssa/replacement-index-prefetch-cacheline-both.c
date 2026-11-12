/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-options "-O3 -fno-tree-vectorize -fprefetch-loop-arrays -ftree-replacement-index-prefetch --param=l1-cache-line-size=96 -Wno-disabled-optimization -fchecking=2 -fdump-tree-aprefetch-details" } */

/* Rejecting the legacy path must preserve the new path's SSA update.  */
#include "replacement-index-prefetch-1.c"

/* { dg-final { scan-tree-dump-times "Replacement-index prefetch:" 1 "aprefetch" } } */
