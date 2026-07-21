/* { dg-do run { target { aarch64*-*-* || { x86_64-*-* && { ! ia32 } } } } } */
/* { dg-options "-O2 -fno-tree-cselim -fdebruijn-clz -fdump-tree-optimized" } */
/* { dg-additional-options "-mbmi -mlzcnt" { target { x86_64-*-* && { ! ia32 } } } } */
/* { dg-require-effective-target ctzll } */
/* { dg-require-effective-target clzll } */

#if defined (__x86_64__)
#include <cpuid.h>
#endif

typedef unsigned int U32;
typedef unsigned long long U64;

static inline unsigned
ctz32_fallback (U32 val)
{
  static const U32 table[32] = {
    0, 1, 28, 2, 29, 14, 24, 3,
    30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7,
    26, 12, 18, 6, 11, 5, 10, 9
  };
  return table[((U32) ((val & ((~val) + 1)) * 0x077CB531U)) >> 27];
}

static inline unsigned
clz32_fallback (U32 val)
{
  static const U32 table[32] = {
    0, 9, 1, 10, 13, 21, 2, 29,
    11, 14, 16, 18, 22, 25, 3, 30,
    8, 12, 20, 28, 15, 17, 24, 7,
    19, 27, 23, 6, 26, 5, 4, 31
  };
  val |= val >> 1;
  val |= val >> 2;
  val |= val >> 4;
  val |= val >> 8;
  val |= val >> 16;
  return 31 - table[(val * 0x07C4ACDDU) >> 27];
}

__attribute__((noinline, noclone))
unsigned
ctz32 (U32 val)
{
  return ctz32_fallback (val);
}

__attribute__((noinline, noclone))
unsigned
clz32 (U32 val)
{
  return clz32_fallback (val);
}

__attribute__((noinline, noclone))
unsigned
ctz64 (U64 val)
{
  U32 high = (U32) (val >> 32);
  U32 low = (U32) val;
  if (low == 0)
    return 32 + ctz32_fallback (high);
  return ctz32_fallback (low);
}

__attribute__((noinline, noclone))
unsigned
clz64 (U64 val)
{
  U32 high = (U32) (val >> 32);
  U32 low = (U32) val;
  if (high == 0)
    return 32 + clz32_fallback (low);
  return clz32_fallback (high);
}

/* These forms leave the 32-bit operation's value at zero unnormalized.
   They must not be combined, because the 64-bit replacement preserves the
   zero result of the original split-word implementation.  */
__attribute__((noinline, noclone))
unsigned
ctz64_bare (U64 val)
{
  U32 high = (U32) (val >> 32);
  U32 low = (U32) val;
  if (low == 0)
    return 32 + __builtin_ctz (high);
  return __builtin_ctz (low);
}

__attribute__((noinline, noclone))
unsigned
clz64_bare (U64 val)
{
  U32 high = (U32) (val >> 32);
  U32 low = (U32) val;
  if (high == 0)
    return 32 + __builtin_clz (low);
  return __builtin_clz (high);
}

int
main (void)
{
  unsigned int i;

#if defined (__x86_64__)
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx)
      || !(ebx & bit_BMI)
      || !__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx)
      || !(ecx & bit_LZCNT))
    return 0;
#endif

  if (ctz32 (0) != 0
      || clz32 (0) != 31
      || ctz64 (0) != 32
      || clz64 (0) != 63)
    __builtin_abort ();

  for (i = 0; i < 64; ++i)
    {
      U64 val = 1ULL << i;
      if (ctz64 (val) != (unsigned) __builtin_ctzll (val)
	  || clz64 (val) != (unsigned) __builtin_clzll (val))
	__builtin_abort ();
    }

  if (ctz64 (~0ULL) != 0 || clz64 (~0ULL) != 0)
    __builtin_abort ();
  return 0;
}

/* The two bare functions remain split into two 32-bit operations.  */
/* { dg-final { scan-tree-dump-times {= \.CTZ} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {= \.CLZ} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {& 31} 1 "optimized" } } */
