/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-options "-O3 -fno-tree-vectorize -fprefetch-loop-arrays -fno-tree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */

/* Enabling the old path must not implicitly enable the replacement path.  */
#include "replacement-index-prefetch-1.c"

/* { dg-final { scan-tree-dump-not "Replacement-index prefetch:" "aprefetch" } } */
