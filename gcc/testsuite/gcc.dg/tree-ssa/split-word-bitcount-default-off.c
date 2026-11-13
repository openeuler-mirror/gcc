/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include "split-word-bitcount-2.c"

/* The default must match an explicit -fno-debruijn-clz.  */
/* { dg-final { scan-tree-dump-times {= \.CTZ} 3 "optimized" } } */
/* { dg-final { scan-tree-dump-not {\.CLZ|__builtin_clz} "optimized" } } */
/* { dg-final { scan-tree-dump {& 31} "optimized" } } */
