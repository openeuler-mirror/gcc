/* Body-only bases are not accessed when the inner loop is empty.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing -fno-ipa-cp -fno-ipa-cp-clone -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
int row_count;
#define BITSET_NAME empty_inner_loop
#define BITSET_ROWS row_count
#include "bitset-store-coalescing-kernel.h"
int
main (void)
{
  struct int_vector l = { 2, 2, 0 };
  struct bitset_context ctx = { 1, 0, 0 };
  empty_inner_loop (&ctx, 0, &l);
}
/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path" 1 "bitset_coalesce" } } */
