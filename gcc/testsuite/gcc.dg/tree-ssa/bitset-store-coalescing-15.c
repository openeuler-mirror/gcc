/* Local GIMPLE passes observe the options restored for each function.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-icf -fno-tree-bitset-store-coalescing -fdump-tree-all-details" } */
#define BITSET_NAME enabled_update
#define BITSET_ATTRIBUTES noinline, noclone, optimize ("tree-bitset-store-coalescing")
#include "bitset-store-coalescing-kernel.h"
#define BITSET_NAME disabled_update
#define BITSET_ATTRIBUTES noinline, noclone, optimize ("no-tree-bitset-store-coalescing")
#include "bitset-store-coalescing-kernel.h"
/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path in enabled_update" 1 "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path in disabled_update" "bitset_coalesce" } } */
