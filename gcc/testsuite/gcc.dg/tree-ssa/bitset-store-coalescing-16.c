/* An unsupported indexed metadata read must not block a later candidate.  */
/* { dg-do compile } */
/* { dg-options "-O3 -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
typedef unsigned long long word;
struct word_vector { int size, capacity; word *data; };
struct int_vector { int size, capacity; int *data; };
struct bitset_context { int words; struct word_vector *values, *presence; };

#define NEST(CONTEXT, LITERALS, ROW)                                      \
do {                                                                   \
  struct bitset_context *ctx = (CONTEXT);                                \
  struct int_vector *literals = (LITERALS);                              \
  int limit = literals->size < 64 * ctx->words - 1                        \
              ? literals->size : 64 * ctx->words - 1;                    \
  for (int k = 0; k < limit; ++k)                                       \
    {                                                                  \
      int local_bit = (rotation + k + 1) % limit + 1;                     \
      for (int i = 0; i < literals->size; ++i)                           \
        {                                                              \
          int literal = literals->data[i];                              \
          int row = (ROW);                                             \
          unsigned *values = (unsigned *) (ctx->values->data              \
                                           + ctx->words * row);        \
          unsigned *presence = (unsigned *) (ctx->presence->data          \
                                             + ctx->words * row);      \
          unsigned shift = local_bit & 31;                              \
          unsigned bit = 1U << shift;                                   \
          if ((presence[local_bit >> 5] & bit) != 0)                     \
            continue;                                                  \
          int old = (values[local_bit >> 5] >> shift) & 1;               \
          int desired = ((i == k) ^ literal) & 1;                        \
          if (old != desired)                                          \
            values[local_bit >> 5] ^= bit;                              \
        }                                                              \
    }                                                                  \
} while (0)

void
two_nests_a (struct bitset_context *a, struct int_vector *x,
             struct bitset_context *b, struct int_vector *y,
             int rotation, const int *indices)
{
  NEST (a, x, indices[literal >> 1]);
  NEST (b, y, literal >> 1);
}

void
two_nests_b (struct bitset_context *a, struct int_vector *x,
             struct bitset_context *b, struct int_vector *y,
             int rotation, const int *indices)
{
  NEST (a, x, literal >> 1);
  NEST (b, y, indices[literal >> 1]);
}

/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path in two_nests_" 2 "bitset_coalesce" } } */
/* { dg-final { scan-tree-dump "candidate transform rejected before CFG mutation" "bitset_coalesce" } } */
