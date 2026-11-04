/* Shared canonical kernel, with deliberately noncanonical variants.
   Runtime users disable strict aliasing for its intentional 64/32-bit view.  */
#ifndef BITSET_TEST_TYPES
#define BITSET_TEST_TYPES
typedef unsigned long long word;
struct word_vector { int size, capacity; word *data; };
struct int_vector { int size, capacity; int *data; };
struct bitset_context
{
  int words;
  struct word_vector *values, *presence;
};
#endif

#ifndef BITSET_INDEX
#define BITSET_INDEX ((rotation + k + 1) % limit + 1)
#endif
#ifndef BITSET_DESIRED
#define BITSET_DESIRED (((i == k) ^ literal) & 1)
#endif
#ifndef BITSET_FLIP
#define BITSET_FLIP bit
#endif
#ifndef BITSET_ROWS
#define BITSET_ROWS literals->size
#endif
#ifndef BITSET_EXTRA
#define BITSET_EXTRA
#endif
#ifndef BITSET_ATTRIBUTES
#define BITSET_ATTRIBUTES noinline, noclone
#endif

__attribute__ ((BITSET_ATTRIBUTES)) void
BITSET_NAME (struct bitset_context *ctx, int rotation,
	     struct int_vector *literals)
{
  int limit = literals->size < 64 * ctx->words - 1
	      ? literals->size : 64 * ctx->words - 1;
  for (int k = 0; k < limit; ++k)
    {
      int local_bit = BITSET_INDEX;
      for (int i = 0; i < BITSET_ROWS; ++i)
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
	  BITSET_EXTRA;
	  if ((presence[local_bit >> 5] & bit) != 0)
	    continue;
	  int old_value = (int) ((values[local_bit >> 5] >> shift) & 1U);
	  int desired = BITSET_DESIRED;
	  if (desired != old_value)
	    values[local_bit >> 5] ^= BITSET_FLIP;
	}
    }
}

#undef BITSET_NAME
#undef BITSET_INDEX
#undef BITSET_DESIRED
#undef BITSET_FLIP
#undef BITSET_ROWS
#undef BITSET_EXTRA
#undef BITSET_ATTRIBUTES
