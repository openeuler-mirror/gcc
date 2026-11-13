/* No optimization level enables this pass without its explicit option.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-all" } */
#include "bitset-store-coalescing-1.c"
/* { dg-final { if { [llength [glob -nocomplain "bitset-store-coalescing-12.c.*.bitset_coalesce"]] == 0 } { pass "bitset store coalescing disabled by default" } else { fail "bitset store coalescing disabled by default" } } } */
