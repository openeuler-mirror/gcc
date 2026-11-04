/* Ordinary memory stores are observable even without a volatile qualifier.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing -fno-ipa-cp -fno-ipa-cp-clone -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
int *audit_ptr;
#define BITSET_NAME ordinary_extra_store
#define BITSET_EXTRA audit_ptr[i] = k
#include "bitset-store-coalescing-kernel.h"
int
main (void)
{
  word values[2] = { 0, 0 }, presence[2] = { 0, 0 };
  int data[2] = { 0, 2 }, audit[2] = { -1, -1 };
  struct word_vector v = { 2, 2, values }, p = { 2, 2, presence };
  struct int_vector l = { 2, 2, data };
  struct bitset_context ctx = { 1, &v, &p };
  audit_ptr = audit;
  ordinary_extra_store (&ctx, 0, &l);
  if (audit[0] != 1 || audit[1] != 1)
    __builtin_abort ();
}
/* { dg-final { scan-tree-dump-not "created guarded packed-bit" "bitset_coalesce" } } */
