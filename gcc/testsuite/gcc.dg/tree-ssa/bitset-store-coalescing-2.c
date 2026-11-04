/* An explicitly disabled pass must not execute or produce its own dump.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-bitset-store-coalescing -fdump-tree-all" } */
#include "bitset-store-coalescing-1.c"
/* { dg-final { if { [llength [glob -nocomplain "bitset-store-coalescing-2.c.*.bitset_coalesce"]] == 0 } { pass "bitset store coalescing explicitly disabled" } else { fail "bitset store coalescing explicitly disabled" } } } */
