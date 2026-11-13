/* Exercise multi-word masks and unequal but overlapping storage bases.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing -fno-ipa-cp -fno-ipa-cp-clone -fno-ipa-icf -ftree-bitset-store-coalescing -fdump-tree-bitset_coalesce-details" } */

#include <string.h>
#include <stdio.h>

/* The canonical kernel uses 64-bit rows viewed as 32-bit packed words.
   Disable strict aliasing for these intentional views.  */
#define BITSET_TEST_TYPES
typedef unsigned long long word;
struct word_vector { int size, capacity; word *data; };
struct int_vector { int size, capacity; int *data; };
struct bitset_context
{
  int words;
  struct word_vector *values, *presence;
};

#define BITSET_NAME optimized_update
#include "bitset-store-coalescing-kernel.h"
#define BITSET_NAME reference_update
#define BITSET_ATTRIBUTES noinline, noclone, optimize ("no-tree-bitset-store-coalescing")
#include "bitset-store-coalescing-kernel.h"

static unsigned
next_value (unsigned *state)
{
  *state = *state * 1664525U + 1013904223U;
  return *state;
}

static void
run_case (int count, int presence_offset, int literal_overlap, int duplicates)
{
  enum { WORDS = 4, ROWS = 128, STORAGE = WORDS * ROWS + 16 };
  word original[STORAGE], changed[STORAGE];
  word masks[STORAGE], changed_masks[STORAGE];
  int original_literals[96], changed_literals[96];

  for (int rotation = 0; rotation < count; ++rotation)
    {
      unsigned state = 17U + rotation;
      for (int i = 0; i < STORAGE; ++i)
        {
          original[i] = ((word) next_value (&state) << 32)
                        | next_value (&state);
          masks[i] = ((word) next_value (&state) << 32)
                     | next_value (&state);
        }
      for (int i = 0; i < count; ++i)
        original_literals[i] = 2 * (duplicates ? i % 4 : i) + (i & 1);

      struct word_vector values = { WORDS * ROWS, WORDS * ROWS,
                                    original + 8 };
      struct word_vector presence = { WORDS * ROWS, WORDS * ROWS,
                                      presence_offset == 99 ? masks + 8
                                      : values.data + presence_offset };
      struct int_vector literals = { count, count, original_literals };
      if (literal_overlap)
        {
          /* With count == 2, every modified row number stays inside ROWS.  */
	  if (literal_overlap == 2)
	    {
	      /* The literal interval crosses the writable base itself.  */
	      literals.data = (int *) values.data - 1;
	      literals.data[0] = 0;
	      literals.data[1] = 2;
	      values.data[2 * WORDS] |= 4;
	      values.data[3 * WORDS] |= 4;
	      memset (masks, 0, sizeof masks);
	    }
	  else
	    {
	      literals.data = (int *) (values.data + WORDS);
	      literals.data[0] = 2;
	      literals.data[1] = 4;
	    }
        }
      memcpy (changed, original, sizeof original);
      memcpy (changed_masks, masks, sizeof masks);
      memcpy (changed_literals, original_literals, sizeof original_literals);
      struct word_vector new_values = { values.size, values.capacity,
                                        changed + 8 };
      struct word_vector new_presence = { presence.size, presence.capacity,
                                          presence_offset == 99
                                          ? changed_masks + 8
                                          : new_values.data + presence_offset };
      struct int_vector new_literals = { count, count,
                                         literal_overlap
                                         ? (literal_overlap == 2
					    ? (int *) new_values.data - 1
					    : (int *) (new_values.data + WORDS))
                                         : changed_literals };
      struct bitset_context a = { WORDS, &values, &presence };
      struct bitset_context b = { WORDS, &new_values, &new_presence };
      reference_update (&a, rotation, &literals);
      optimized_update (&b, rotation, &new_literals);
      if (memcmp (original, changed, sizeof original)
          || memcmp (masks, changed_masks, sizeof masks))
        {
          fprintf (stderr, "count=%d rotation=%d offset=%d literal=%d duplicate=%d\n",
                   count, rotation, presence_offset, literal_overlap, duplicates);
          __builtin_abort ();
        }
    }
}

int
main (void)
{
  static const int counts[] = { 1, 16, 31, 32, 33, 63, 64, 65, 95, 96 };
  for (unsigned i = 0; i < sizeof counts / sizeof counts[0]; ++i)
    for (int duplicate = 0; duplicate != 2; ++duplicate)
      {
        run_case (counts[i], 99, 0, duplicate);
        run_case (counts[i], 0, 0, duplicate);
        run_case (counts[i], 1, 0, duplicate);
        run_case (counts[i], -1, 0, duplicate);
      }
  run_case (2, 99, 1, 0);
  run_case (2, 99, 2, 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "created guarded packed-bit word-store fast path" 1 "bitset_coalesce" } } */
