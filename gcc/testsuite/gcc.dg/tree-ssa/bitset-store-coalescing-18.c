/* A folded constant MEM_REF offset is not part of the modeled bit index.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-icf -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
typedef unsigned long long word;
struct word_vector { int size, capacity; word *data; };
struct int_vector { int size, capacity; int *data; };
struct bitset_context { int words; struct word_vector *values, *presence; };

#define DEFINE_OFFSET(NAME, VALUE_DELTA, PRESENCE_DELTA, LITERAL_DELTA)       \
void NAME (struct bitset_context *ctx, int rotation, struct int_vector *lits) \
{                                                                         \
  int limit = lits->size < 64 * ctx->words - 1                              \
              ? lits->size : 64 * ctx->words - 1;                          \
  for (int k = 0; k < limit; ++k)                                         \
    {                                                                     \
      int local_bit = (rotation + k + 1) % limit + 1;                       \
      for (int i = 0; i < lits->size; ++i)                                 \
        {                                                                 \
          int literal = lits->data[i + LITERAL_DELTA];                     \
          word *vr = ctx->values->data + ctx->words * (literal >> 1);        \
          word *pr = ctx->presence->data + ctx->words * (literal >> 1);      \
          unsigned *values = (unsigned *) vr;                              \
          unsigned *presence = (unsigned *) pr;                            \
          unsigned shift = local_bit & 31, bit = 1U << shift;              \
          if (presence[(local_bit >> 5) + PRESENCE_DELTA] & bit)            \
            continue;                                                     \
          int old = (values[(local_bit >> 5) + VALUE_DELTA] >> shift) & 1;  \
          int desired = ((i == k) ^ literal) & 1;                          \
          if (old != desired)                                             \
            values[(local_bit >> 5) + VALUE_DELTA] ^= bit;                  \
        }                                                                 \
    }                                                                     \
}

DEFINE_OFFSET (offset_control, 0, 0, 0)
DEFINE_OFFSET (offset_value, 1, 0, 0)
DEFINE_OFFSET (offset_presence, 0, 1, 0)
DEFINE_OFFSET (offset_literal, 0, 0, 1)

/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path in offset_control" 1 "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path in offset_value" "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path in offset_presence" "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump-not "created guarded packed-bit word-store fast path in offset_literal" "bitset_coalesce" } } */
