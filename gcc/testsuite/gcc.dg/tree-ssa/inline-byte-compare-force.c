/* { dg-do compile } */
/* { dg-options "-O2 -finline-stringops=memcmp -finline-memcmp" } */
__attribute__((noipa)) int ordered (const void *a, const void *b, __SIZE_TYPE__ n)
{ return __builtin_memcmp (a, b, n); }
__attribute__((noipa)) int equal (const void *a, const void *b, __SIZE_TYPE__ n)
{ return __builtin_memcmp (a, b, n) == 0; }
/* { dg-final { scan-assembler-not "\\mmemcmp\\M" } } */
