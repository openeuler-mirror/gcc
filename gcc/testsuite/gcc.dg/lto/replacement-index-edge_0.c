/* { dg-lto-do run } */
/* { dg-skip-if "AArch64 prefetch" { ! aarch64*-*-linux* } } */
/* { dg-lto-options { { -O3 -fno-tree-vectorize -fno-prefetch-loop-arrays -ftree-replacement-index-prefetch -fchecking=2 -flto -flto-partition=one } } } */
#include "../tree-ssa/replacement-index-edge-run.c"
