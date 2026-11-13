/* Neither masked-off nor already-correct words may acquire extra writes.
   In particular, setting a diagonal to its base bit then flipping it back
   still introduces a store when the source performs none.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing -fno-ipa-cp -fno-ipa-cp-clone -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
#define BITSET_NAME unchanged_readonly_words
#include "bitset-store-coalescing-kernel.h"

union readonly_storage
{
  word rows[2];
  unsigned bits[4];
};
static const union readonly_storage all_zero = { { 0, 0 } };
/* Specify the 32-bit view so the case is independent of byte order.  */
static const union readonly_storage already_correct
  = { .bits = { 4, 0, 2, 0 } };
int
main (void)
{
  word presence[2] = { ~0ULL, ~0ULL };
  int data[2] = { 0, 2 };
  struct word_vector v = { 2, 2, (word *) all_zero.rows };
  struct word_vector p = { 2, 2, presence };
  struct int_vector l = { 2, 2, data };
  struct bitset_context ctx = { 1, &v, &p };
  unchanged_readonly_words (&ctx, 0, &l);
  v.data = (word *) already_correct.rows;
  presence[0] = presence[1] = 0;
  unchanged_readonly_words (&ctx, 0, &l);
  /* No array bases are needed when the outer loop executes zero times.  */
  l.size = 0;
  ctx.values = ctx.presence = 0;
  unchanged_readonly_words (&ctx, 0, &l);
}
/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path" 1 "bitset_coalesce" } } */
