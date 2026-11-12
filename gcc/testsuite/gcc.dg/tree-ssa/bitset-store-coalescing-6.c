/* Failed cloning of an outer-IV-dependent row count must leave the CFG intact.  */
/* { dg-do compile } */
/* { dg-options "-O3 -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
#define BITSET_NAME varying_row_count
#define BITSET_ROWS (literals->size - k)
#include "bitset-store-coalescing-kernel.h"
/* { dg-final { scan-tree-dump-not "created guarded packed-bit" "bitset_coalesce" } } */
