#include <stddef.h>

__attribute__((noinline, noclone)) int
family_equal (const void *a, const void *b, size_t n)
{
  return __builtin_memcmp (a, b, n) == 0;
}

__attribute__((noinline, noclone)) int
family_ordered (const void *a, const void *b, size_t n)
{
  return __builtin_memcmp (a, b, n);
}
