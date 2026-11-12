/* Explicit off overrides a translation-unit on option, too.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-icf -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
#include "bitset-store-coalescing-15.c"
/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path in enabled_update" 1 "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path in disabled_update" "bitset_coalesce" } } */
