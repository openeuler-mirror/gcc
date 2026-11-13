/* Disabling CH must also disable its bitset preparation passes.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-ch -ftree-bitset-store-coalescing -fdump-tree-all" } */
#define BITSET_NAME no_ch_update
#include "bitset-store-coalescing-kernel.h"
/* { dg-final { if { [llength [glob -nocomplain "bitset-store-coalescing-14.c.*bitset_coalesce_prep*"]] == 0 } { pass "bitset preparation respects no-tree-ch" } else { fail "bitset preparation respects no-tree-ch" } } } */
