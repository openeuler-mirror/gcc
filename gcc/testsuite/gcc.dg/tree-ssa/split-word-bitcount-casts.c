/* { dg-do run { target { x86_64-*-* && { ! ia32 } } } } */
/* { dg-require-effective-target bitint } */
/* { dg-options "-O2 -mbmi -fno-tree-cselim -fdebruijn-clz" } */

#include <cpuid.h>

typedef unsigned int U32;
typedef unsigned long long U64;

/* A cast in either arm must preserve the half-word count before the
   diamond can be replaced.  In particular, signed _BitInt(5) maps 20
   to -12, and unsigned _BitInt(4) maps it to 4.  The explicit zero value
   makes ctzg lower to IFN_CTZ during GIMPLE with -mbmi, and keeps the
   add arm's normalization necessary for a zero high half.  */
#define DEFINE_COUNTS(NAME, TYPE) \
__attribute__((noipa)) unsigned \
NAME##_plain (U64 x) \
{ \
  U32 high = (U32) (x >> 32); \
  U32 low = (U32) x; \
  if (low == 0) \
    return 32 + (__builtin_ctzg (high, 32) & 31); \
  return (unsigned) (TYPE) __builtin_ctzg (low, 32); \
} \
__attribute__((noipa)) unsigned \
NAME##_add (U64 x) \
{ \
  U32 high = (U32) (x >> 32); \
  U32 low = (U32) x; \
  if (low == 0) \
    return 32 + (unsigned) (TYPE) (__builtin_ctzg (high, 32) & 31); \
  return __builtin_ctzg (low, 32); \
}

DEFINE_COUNTS (signed5, signed _BitInt(5))
DEFINE_COUNTS (unsigned4, unsigned _BitInt(4))
/* These conversions preserve every value in [0, 31].  */
DEFINE_COUNTS (signed6, signed _BitInt(6))
DEFINE_COUNTS (unsigned5, unsigned _BitInt(5))

#define CHECK_COUNTS(NAME, TYPE) \
do \
  { \
    unsigned plain = i < 32 ? (unsigned) (TYPE) i : i; \
    unsigned add = i < 32 ? i : 32 + (unsigned) (TYPE) (i - 32); \
    if (NAME##_plain (x) != plain || NAME##_add (x) != add) \
      __builtin_abort (); \
  } \
while (0)

int
main (void)
{
  unsigned int eax, ebx, ecx, edx;
  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx)
      || !(ebx & bit_BMI))
    return 0;

  /* Exercise both arms and every count for a nonzero half-word, including
     the signedness and truncation boundaries.  */
  for (unsigned i = 0; i < 64; ++i)
    {
      U64 x = 1ULL << i;
      CHECK_COUNTS (signed5, signed _BitInt(5));
      CHECK_COUNTS (unsigned4, unsigned _BitInt(4));
      CHECK_COUNTS (signed6, signed _BitInt(6));
      CHECK_COUNTS (unsigned5, unsigned _BitInt(5));
    }
  if (signed5_plain (~0ULL) != 0 || signed5_add (~0ULL) != 0
      || unsigned4_plain (~0ULL) != 0 || unsigned4_add (~0ULL) != 0)
    __builtin_abort ();
  return 0;
}
