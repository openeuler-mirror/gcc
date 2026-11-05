/* { dg-do run { target { aarch64*-*-* || { x86_64-*-* && { ! ia32 } } } } } */
/* { dg-options "-O2 -fno-tree-cselim -fdebruijn-clz -fdump-tree-optimized" } */
/* { dg-additional-options "-mbmi -mlzcnt" { target { x86_64-*-* && { ! ia32 } } } } */
/* { dg-require-effective-target ctzll } */
/* { dg-require-effective-target clzll } */

#pragma GCC push_options
#pragma GCC optimize ("no-debruijn-clz")
#include "split-word-bitcount-1.c"
#pragma GCC pop_options

/* { dg-final { scan-tree-dump-times {= \.CTZ} 3 "optimized" } } */
/* { dg-final { scan-tree-dump-not {\.CLZ} "optimized" } } */
