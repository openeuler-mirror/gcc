/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */

#include "bitset-store-coalescing-1.c"

/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path" "bitset_coalesce" } } */
