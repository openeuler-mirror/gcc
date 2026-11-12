/* { dg-do run { target { aarch64*-*-* || { x86_64-*-* && { ! ia32 } } } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-mlzcnt" { target { x86_64-*-* && { ! ia32 } } } } */
/* { dg-require-effective-target clz } */

#if defined (__x86_64__)
#include <cpuid.h>
#endif

#ifndef FUNCTION_OPTIONS
#define FUNCTION_OPTIONS "debruijn-clz"
#endif

/* The function option must override the command-line default.  The
   companion test checks the reverse direction using the same function.  */
unsigned int __attribute__((noipa, optimize (FUNCTION_OPTIONS)))
count_leading_zeroes (unsigned int x)
{
  static const unsigned int table[32] = {
    0, 9, 1, 10, 13, 21, 2, 29,
    11, 14, 16, 18, 22, 25, 3, 30,
    8, 12, 20, 28, 15, 17, 24, 7,
    19, 27, 23, 6, 26, 5, 4, 31
  };
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return 31 - table[(x * 0x07c4acddU) >> 27];
}

int
main (void)
{
#if defined (__x86_64__)
  unsigned int eax, ebx, ecx, edx;
  if (!__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx)
      || !(ecx & bit_LZCNT))
    return 0;
#endif
  if (count_leading_zeroes (0) != 31
      || count_leading_zeroes (~0U) != 0)
    __builtin_abort ();
  for (unsigned int i = 0; i < 32; ++i)
    if (count_leading_zeroes (1U << i) != 31 - i)
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times {= \.CLZ} 1 "optimized" } } */
