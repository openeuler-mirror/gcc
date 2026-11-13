/* Distinct row indices can still select partially overlapping output rows.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing -fno-ipa-cp -fno-ipa-cp-clone -fno-ipa-icf -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
typedef unsigned long long word;
struct word_vector { int size, capacity; word *data; };
struct int_vector { int size, capacity; int *data; };
struct bitset_context { int words; struct word_vector *values, *presence; };

#define DEFINE_UPDATE(NAME, OPTION)                                     \
__attribute__((noinline, noclone, optimize (OPTION)))                     \
static void NAME (struct bitset_context *ctx,                             \
                  struct int_vector *literals, int rotation)             \
{                                                                       \
  int limit = literals->size < 64 * ctx->words - 1                        \
              ? literals->size : 64 * ctx->words - 1;                     \
  for (int k = 0; k < limit; ++k)                                        \
    {                                                                   \
      int local_bit = (rotation + k + 1) % limit + 1;                     \
      for (int i = 0; i < literals->size; ++i)                            \
        {                                                               \
          int literal = literals->data[i];                               \
          unsigned *values = (unsigned *) (ctx->values->data               \
                                           + (literal >> 1));           \
          unsigned *presence = (unsigned *) (ctx->presence->data           \
                                             + (literal >> 1));         \
          unsigned shift = local_bit & 31;                               \
          unsigned bit = 1U << shift;                                    \
          if ((presence[local_bit >> 5] & bit) != 0)                      \
            continue;                                                   \
          int old = (values[local_bit >> 5] >> shift) & 1;                \
          int desired = ((i == k) ^ literal) & 1;                         \
          if (old != desired)                                           \
            values[local_bit >> 5] ^= bit;                               \
        }                                                               \
    }                                                                   \
}

DEFINE_UPDATE (reference, "no-tree-bitset-store-coalescing")
DEFINE_UPDATE (overlapping_rows, "tree-bitset-store-coalescing")

int
main (void)
{
  word before[64] = { 0 }, after[64] = { 0 }, presence[64] = { 0 };
  int input[65];
  input[0] = 0;
  for (int i = 1; i < 65; ++i)
    input[i] = 3;
  struct word_vector a = {64, 64, before}, b = {64, 64, after};
  struct word_vector p = {64, 64, presence};
  struct int_vector literals = {65, 65, input};
  struct bitset_context ca = {2, &a, &p}, cb = {2, &b, &p};
  reference (&ca, &literals, 64);
  overlapping_rows (&cb, &literals, 64);
  if (__builtin_memcmp (before, after, sizeof before))
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path in overlapping_rows" 1 "bitset_coalesce" } } */
