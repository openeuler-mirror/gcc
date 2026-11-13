/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -finline-memcmp-small-equality -fdump-rtl-expand" } */
/* { dg-final { scan-rtl-dump-times "Expanded bounded dynamic equality comparison" 2 "expand" } } */
#include <stddef.h>
int equal (const void *a, const void *b, size_t n)
{ return __builtin_memcmp (a, b, n) == 0; }
int different (const void *a, const void *b, size_t n)
{ return __builtin_memcmp (a, b, n) != 0; }
int ordered (const void *a, const void *b, size_t n)
{ return __builtin_memcmp (a, b, n) < 0; }
int constant (const void *a, const void *b)
{ return __builtin_memcmp (a, b, 7) == 0; }
