/* An equivalent continue must not be rejected by an exact block count.  */
/* { dg-do compile } */
/* { dg-options "-O3 -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
#define BITSET_NAME update_continue
#define BITSET_CONTINUE
#include "bitset-store-coalescing-kernel.h"
/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path" 1 "bitset_coalesce" } } */
