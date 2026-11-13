/* Additional observable operations cannot be dropped by loop interchange.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing -fno-ipa-cp -fno-ipa-cp-clone -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
volatile int audit_counter = -1;
#define BITSET_NAME observable_store
#define BITSET_EXTRA audit_counter = k
#include "bitset-store-coalescing-kernel.h"

int
main (void)
{
  word values[2] = { 0, 0 }, presence[2] = { 0, 0 };
  int data[2] = { 0, 2 };
  struct word_vector v = { 2, 2, values }, p = { 2, 2, presence };
  struct int_vector l = { 2, 2, data };
  struct bitset_context ctx = { 1, &v, &p };
  observable_store (&ctx, 0, &l);
  if (audit_counter != 1)
    __builtin_abort ();
}
/* { dg-final { scan-tree-dump-not "created guarded packed-bit" "bitset_coalesce" } } */
