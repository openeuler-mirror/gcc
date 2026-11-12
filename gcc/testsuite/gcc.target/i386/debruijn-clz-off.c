/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -mlzcnt -fno-debruijn-clz -fdump-tree-forwprop1-details" } */

#include "debruijn-clz-default-off.c"

/* { dg-final { scan-tree-dump-times {= \.CTZ} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not {\.CLZ|__builtin_clz} "forwprop1" } } */
