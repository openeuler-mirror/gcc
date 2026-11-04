/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-O3 -fno-tree-vectorize -fchecking=2 -fdump-tree-aprefetch-details" } */
#include "replacement-index-edge.h"
/* { dg-final { scan-assembler-not "prfm" } } */
