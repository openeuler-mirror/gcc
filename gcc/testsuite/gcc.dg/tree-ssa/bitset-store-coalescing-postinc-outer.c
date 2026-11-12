/* A bottom test of the old IV executes one extra update.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
typedef unsigned long long word;
struct vec { int size; word *data; };
struct ivec { int size; int *data; };
struct ctx_t { int words; struct vec *values, *presence; };

#define KERNEL(NAME, OPT)                                                   \
__attribute__ ((noinline, noclone, optimize (OPT))) void                    \
NAME (struct ctx_t *ctx, int rotation, struct ivec *lits)                   \
{                                                                           \
int limit = lits->size < 64 * ctx->words - 1 ? lits->size : 64 * ctx->words - 1; \
int k = 0;                                                                \
do                                                                        \
  {                                                                       \
    int local_bit = (rotation + k + 1) % limit + 1;                       \
    for (int i = 0; i < lits->size; ++i)                                  \
      {                                                                   \
        int literal = lits->data[i];                                      \
        unsigned *values = (unsigned *) (ctx->values->data + ctx->words * (literal >> 1)); \
        unsigned *presence = (unsigned *) (ctx->presence->data + ctx->words * (literal >> 1)); \
        unsigned shift = local_bit & 31, bit = 1U << shift;               \
        if ((presence[local_bit >> 5] & bit) != 0)                        \
          continue;                                                       \
        int old_value = (int) ((values[local_bit >> 5] >> shift) & 1U);   \
        int desired = ((i == k) ^ literal) & 1;                           \
        if (desired != old_value)                                         \
          values[local_bit >> 5] ^= bit;                                  \
      }                                                                   \
  }                                                                       \
while (k++ < limit);    /* body runs for k = 0 .. limit */                \
}
KERNEL (opt, "tree-bitset-store-coalescing")
KERNEL (ref, "no-tree-bitset-store-coalescing")

int
main (void)
{
word a[8] = { 0 }, b[8] = { 0 }, p[8] = { 0 };
int data[8];
for (int i = 0; i < 8; ++i)
  data[i] = 2 * i + (i & 1);
struct vec va = { 8, a }, vb = { 8, b }, vp = { 8, p };
struct ivec l = { 8, data };
struct ctx_t ca = { 1, &va, &vp }, cb = { 1, &vb, &vp };
ref (&ca, 3, &l);
opt (&cb, 3, &l);
for (int i = 0; i < 8; ++i)
  if (a[i] != b[i])
    {
      __builtin_printf ("row %d: ref %#llx opt %#llx\n", i, a[i], b[i]);
      return 1;
    }
return 0;
}
