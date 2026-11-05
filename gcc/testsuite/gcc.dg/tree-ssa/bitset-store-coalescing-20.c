/* Cloning the bitset preparation instance must preserve ordinary CH passes.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-bitset-store-coalescing -fdump-tree-all" } */
#define BITSET_NAME ordinary_header_copying
#include "bitset-store-coalescing-kernel.h"
/* { dg-final { if { [llength [glob -nocomplain {bitset-store-coalescing-20.c.*.ch[0-9]*}]] > 0 } { pass "ordinary CH still runs with bitset disabled" } else { fail "ordinary CH still runs with bitset disabled" } } } */
