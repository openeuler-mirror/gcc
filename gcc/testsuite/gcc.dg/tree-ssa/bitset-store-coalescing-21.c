/* Address conversions must preserve the nonnegative induction value.  */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -fno-ipa-icf -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
typedef unsigned long long word;
struct word_vector { unsigned size, capacity; word *data; };
struct int_vector { unsigned size, capacity; int *data; };
struct bitset_context { int words; struct word_vector *values, *presence; };
#define DEFINE_UPDATE(NAME, LOAD)                                       \
void NAME (struct bitset_context *ctx, struct int_vector *literals,       \
           unsigned rotation)                                          \
{                                                                      \
  unsigned limit = literals->size < 64U * ctx->words - 1                  \
                   ? literals->size : 64U * ctx->words - 1;             \
  for (unsigned k = 0; k < limit; ++k)                                   \
    {                                                                  \
      unsigned local_bit = (rotation + k + 1) % limit + 1;               \
      for (unsigned i = 0; i < literals->size; ++i)                      \
        {                                                              \
          int literal = (LOAD);                                        \
          unsigned *values = (unsigned *) (ctx->values->data             \
                                     + ctx->words * (literal >> 1));    \
          unsigned *presence = (unsigned *) (ctx->presence->data         \
                                       + ctx->words * (literal >> 1));  \
          unsigned shift = local_bit & 31;                              \
          unsigned bit = 1U << shift;                                   \
          if ((presence[local_bit >> 5] & bit) != 0)                     \
            continue;                                                  \
          int old = (values[local_bit >> 5] >> shift) & 1;               \
          int desired = ((i == k) ^ literal) & 1;                       \
          if (old != desired)                                          \
            values[local_bit >> 5] ^= bit;                              \
        }                                                              \
    }                                                                  \
}
DEFINE_UPDATE (unsigned_input, literals->data[i])
DEFINE_UPDATE (signed_input, literals->data[(int) i])
DEFINE_UPDATE (wrapped_bytes,
               *(int *) ((char *) literals->data + (int) (i * 4U)))
DEFINE_UPDATE (narrow_unsigned_bytes,
               *(int *) ((char *) literals->data + i * 4U))
/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path in unsigned_input" 1 "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path in signed_input" "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path in wrapped_bytes" "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path in narrow_unsigned_bytes" "bitset_coalesce" } } */
