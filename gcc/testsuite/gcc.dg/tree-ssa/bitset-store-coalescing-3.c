/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -ftree-bitset-store-coalescing" } */

#include <string.h>
#include "bitset-store-coalescing-1.c"

__attribute__ ((noinline, optimize ("O3,no-tree-bitset-store-coalescing")))
static void
reference_update (struct bitset_context *ctx, int rotation,
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

static unsigned
next_value (unsigned *state)
{
  *state = *state * 1664525U + 1013904223U;
  return *state;
}

static void
run_case (int duplicate_rows, int alias_arrays)
{
  enum { WORDS = 4, ROWS = 24, LITERALS = 16 };
  word reference_storage[WORDS * ROWS];
  word optimized_storage[WORDS * ROWS];
  word reference_presence[WORDS * ROWS];
  word optimized_presence[WORDS * ROWS];
  int literal_data[LITERALS];
  struct int_vector literals = { LITERALS, LITERALS, literal_data };
  struct word_vector reference_values
    = { WORDS * ROWS, WORDS * ROWS, reference_storage };
  struct word_vector optimized_values
    = { WORDS * ROWS, WORDS * ROWS, optimized_storage };
  struct word_vector reference_masks
    = { WORDS * ROWS, WORDS * ROWS, reference_presence };
  struct word_vector optimized_masks
    = { WORDS * ROWS, WORDS * ROWS, optimized_presence };
  struct bitset_context reference_ctx
    = { WORDS, &reference_values, &reference_masks };
  struct bitset_context optimized_ctx
    = { WORDS, &optimized_values, &optimized_masks };
  unsigned state = 1;

  for (int i = 0; i < WORDS * ROWS; ++i)
    {
      reference_storage[i] = ((word) next_value (&state) << 32)
			     | next_value (&state);
      reference_presence[i] = ((word) next_value (&state) << 32)
			      | next_value (&state);
    }
  memcpy (optimized_storage, reference_storage, sizeof reference_storage);
  memcpy (optimized_presence, reference_presence, sizeof reference_presence);

  for (int i = 0; i < LITERALS; ++i)
    literal_data[i] = duplicate_rows ? ((i % 4) * 2 + (i & 1))
				     : (i * 2 + (i & 1));

  if (alias_arrays)
    {
      reference_ctx.presence = &reference_values;
      optimized_ctx.presence = &optimized_values;
    }

  for (int rotation = 0; rotation < LITERALS; ++rotation)
    {
      reference_update (&reference_ctx, rotation, &literals);
      update_packed_bits (&optimized_ctx, rotation, &literals);
      if (memcmp (reference_storage, optimized_storage,
		  sizeof reference_storage) != 0)
	__builtin_abort ();
    }
}

int
main (void)
{
  run_case (0, 0);
  run_case (1, 0);
  run_case (0, 1);
  return 0;
}
