/* Verify that an explicit disable overrides a preceding enable.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fbit-scan-loop-idiom -fno-bit-scan-loop-idiom -fdump-tree-optimized" } */

#include "bit-scan-loop-idiom-2.c"

/* { dg-final { scan-tree-dump-not "__builtin_ctz|\\.CTZ|__builtin_clz|\\.CLZ" "optimized" } } */
