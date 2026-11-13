/* Do not strengthen the alias set of a packed word access.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-strict-aliasing -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */
typedef unsigned long long word;
typedef unsigned u32u __attribute__ ((may_alias));
struct vec { int size; word *data; };
struct ivec { int size; int *data; };
struct ctx_t { int words; struct vec *values, *presence; };

void
f (struct ctx_t *ctx, int rotation, struct ivec *lits, int *out)
{
int limit = lits->size < 64 * ctx->words - 1 ? lits->size : 64 * ctx->words - 1;
int k = 0, local_bit;
do
  {
    local_bit = (rotation + k + 1) % limit + 1;
    for (int i = 0; i < lits->size; ++i)
      {
        int literal = lits->data[i];
        u32u *values = (u32u *) ((char *) ctx->values->data + ctx->words * (literal >> 1));
        u32u *presence = (u32u *) ((char *) ctx->presence->data + ctx->words * (literal >> 1));
        unsigned shift = local_bit & 31, bit = 1U << shift;
        if ((presence[local_bit >> 5] & bit) != 0)
          continue;
        int old_value = (int) ((values[local_bit >> 5] >> shift) & 1U);
        int desired = ((i == k) ^ literal) & 1;
        if (desired != old_value)
          values[local_bit >> 5] ^= bit;
      }
  }
while (++k < limit);
(void) out;          /* No semantic scalar live-out.  */
}

/* { dg-final { scan-tree-dump-not "created guarded packed-bit" "bitset_coalesce" } } */
