/* A row guard is not part of the coalesced update semantics.  */
/* { dg-do compile } */
/* { dg-options "-O3 -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
#define BITSET_NAME guarded_update
#define BITSET_EXTRA if (literal & 4) continue
#include "bitset-store-coalescing-kernel.h"
/* { dg-final { scan-tree-dump-not "created guarded packed-bit" "bitset_coalesce" } } */
