/* The preparation pass must not renumber ordinary header copying.  */
/* { dg-do compile } */
/* { dg-options "-O3 -ftree-bitset-store-coalescing -fdump-tree-all-details" } */

#define BITSET_NAME separate_header_copying
#include "bitset-store-coalescing-kernel.h"

/* { dg-final { scan-tree-dump "is now do-while loop" "ch_bitset" } } */
/* { dg-final { scan-tree-dump "separate_header_copying" "ch2" } } */
/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path" 1 "bitset_coalesce" } } */
