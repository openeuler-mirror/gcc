/* Signed remainders, nonunit permutations and non-bit desired values.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing -fno-ipa-cp -fno-ipa-cp-clone -ftree-bitset-store-coalescing" } */
#define BITSET_NAME signed_rotation
#include "bitset-store-coalescing-kernel.h"
#define BITSET_NAME signed_reference
#define BITSET_ATTRIBUTES noinline, noclone, optimize ("no-tree-bitset-store-coalescing")
#include "bitset-store-coalescing-kernel.h"

#define BITSET_NAME nonunit_index
#define BITSET_INDEX ((rotation + 2 * k + 1) % limit + 1)
#include "bitset-store-coalescing-kernel.h"
#define BITSET_NAME nonunit_reference
#define BITSET_INDEX ((rotation + 2 * k + 1) % limit + 1)
#define BITSET_ATTRIBUTES noinline, noclone, optimize ("no-tree-bitset-store-coalescing")
#include "bitset-store-coalescing-kernel.h"

#define BITSET_NAME nonbit_desired
#define BITSET_DESIRED (((i == k) ^ literal) & 3)
#include "bitset-store-coalescing-kernel.h"
#define BITSET_NAME nonbit_reference
#define BITSET_DESIRED (((i == k) ^ literal) & 3)
#define BITSET_ATTRIBUTES noinline, noclone, optimize ("no-tree-bitset-store-coalescing")
#include "bitset-store-coalescing-kernel.h"

typedef void (*kernel) (struct bitset_context *, int, struct int_vector *);
static void
compare (kernel optimized, kernel reference, int rotation, int n)
{
  word actual[4] = { 0, 0, 0, 0 }, expected[4] = { 0, 0, 0, 0 };
  word presence[4] = { 0, 0, 0, 0 };
  int data[4] = { 0, 2, 4, 6 };
  struct word_vector a = { 4, 4, actual }, e = { 4, 4, expected };
  struct word_vector p = { 4, 4, presence };
  struct int_vector l = { n, n, data };
  struct bitset_context ac = { 1, &a, &p }, ec = { 1, &e, &p };
  reference (&ec, rotation, &l);
  optimized (&ac, rotation, &l);
  if (__builtin_memcmp (actual, expected, sizeof actual))
    __builtin_abort ();
}
int
main (void)
{
  /* The visited signed indices are 1 and 0, both valid.  */
  compare (signed_rotation, signed_reference, -3, 2);
  compare (nonunit_index, nonunit_reference, 0, 4);
  compare (nonbit_desired, nonbit_reference, 0, 2);
}
