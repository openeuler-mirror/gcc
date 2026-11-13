/* { dg-do compile } */
/* { dg-options "-O3 -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */

typedef unsigned long long word;

struct word_vector
{
  int size;
  int capacity;
  word *data;
};

struct int_vector
{
  int size;
  int capacity;
  int *data;
};

struct bitset_context
{
  int words;
  struct word_vector *values;
  struct word_vector *presence;
};

__attribute__ ((noinline)) void
update_packed_bits (struct bitset_context *ctx, int rotation,
		    struct int_vector *literals)
{
  int k;
  int limit = literals->size < 64 * ctx->words - 1
	      ? literals->size : 64 * ctx->words - 1;

  for (k = 0; k < limit; ++k)
    {
      int i;
      int local_bit = (rotation + k + 1) % limit + 1;

      for (i = 0; i < literals->size; ++i)
	{
	  int literal = literals->data[i];
	  word *value_row = ctx->values->data
			    + ctx->words * (literal >> 1);
	  word *presence_row = ctx->presence->data
			       + ctx->words * (literal >> 1);
	  unsigned *values = (unsigned *) value_row;
	  unsigned *presence = (unsigned *) presence_row;
	  unsigned shift = local_bit & 31;
	  unsigned bit = 1U << shift;
	  int old_value;
	  int desired;

	  if ((presence[local_bit >> 5] & bit) != 0)
	    continue;
	  old_value = (int) ((values[local_bit >> 5] >> shift) & 1U);
	  desired = ((i == k) ^ literal) & 1;
	  if (desired != old_value)
	    values[local_bit >> 5] ^= bit;
	}
    }
}

/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path" 1 "bitset_coalesce" } } */
