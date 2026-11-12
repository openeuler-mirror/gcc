/* Cloning the bitset preparation instance must preserve ordinary CH passes.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-bitset-store-coalescing -fdump-tree-all" } */
#define BITSET_NAME ordinary_header_copying
#include "bitset-store-coalescing-kernel.h"
/* { dg-final { scan-tree-dump "ordinary_header_copying" "ch2" } } */
