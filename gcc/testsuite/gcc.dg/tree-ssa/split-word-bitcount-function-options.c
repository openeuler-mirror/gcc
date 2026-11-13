/* { dg-do run { target { aarch64*-*-* || { x86_64-*-* && { ! ia32 } } } } } */
/* { dg-options "-O2 -fchecking=2 -fno-tree-cselim -fno-debruijn-clz -fdump-tree-optimized" } */
/* { dg-additional-options "-mbmi -mlzcnt" { target { x86_64-*-* && { ! ia32 } } } } */
/* { dg-require-effective-target ctzll } */
/* { dg-require-effective-target clzll } */

#pragma GCC push_options
#pragma GCC optimize ("debruijn-clz")
#include "split-word-bitcount-1.c"
#pragma GCC pop_options

/* { dg-final { scan-tree-dump-times {= \.CTZ} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {= \.CLZ} 2 "optimized" } } */
