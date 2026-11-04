/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fno-tree-replacement-index-prefetch" } */
#include "replacement-index-prefetch-1.c"
/* { dg-final { scan-assembler-not "prfm" } } */
