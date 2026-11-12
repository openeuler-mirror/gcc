/* { dg-do run { target { aarch64*-*-* || { x86_64-*-* && { ! ia32 } } } } } */
/* { dg-options "-O2 -fdebruijn-clz -fdump-tree-optimized" } */
/* { dg-additional-options "-mlzcnt" { target { x86_64-*-* && { ! ia32 } } } } */
/* { dg-require-effective-target clz } */

#define FUNCTION_OPTIONS "no-debruijn-clz"
#include "debruijn-clz-function-options.c"

/* { dg-final { scan-tree-dump-not {\.CLZ|__builtin_clz} "optimized" } } */
