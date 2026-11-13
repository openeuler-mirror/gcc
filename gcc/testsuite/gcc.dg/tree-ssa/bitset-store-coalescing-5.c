/* Reject an incomplete store match without dereferencing a null statement.  */
/* { dg-do compile } */
/* { dg-options "-O3 -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
#define BITSET_NAME constant_xor_store
#define BITSET_FLIP 1U
#include "bitset-store-coalescing-kernel.h"
/* { dg-final { scan-tree-dump-not "created guarded packed-bit" "bitset_coalesce" } } */
