/* { dg-do run { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target non_strict_align } */
/* { dg-options "-O2 -finline-memcmp-small-equality -fdump-rtl-expand" } */
#include <stddef.h>

__attribute__((noipa)) int equal (const void *a, const void *b, size_t n)
{ return __builtin_memcmp (a, b, n) == 0; }
__attribute__((noipa)) int different (const void *a, const void *b, size_t n)
{ return __builtin_memcmp (a, b, n) != 0; }
__attribute__((noipa)) int ordered (const void *a, const void *b, size_t n)
{ return __builtin_memcmp (a, b, n); }

static unsigned ac, bc, nc;
__attribute__((noipa)) int once (const void *a, const void *b, size_t n)
{
  return __builtin_memcmp ((++ac, a), (++bc, b), (++nc, n)) == 0;
}

int main (void)
{
  unsigned char a[128], b[128];
  for (unsigned x = 0; x != 16; ++x)
    for (unsigned y = 0; y != 16; ++y)
      for (unsigned n = 0; n != 97; ++n)
        {
          for (unsigned i = 0; i != n; ++i)
            a[x+i] = b[y+i] = (unsigned char) (i * 37 + 9);
          if (!equal (a+x, b+y, n) || different (a+x, b+y, n)
              || ordered (a+x, b+y, n) != 0 || !once (a+x, b+y, n))
            __builtin_abort ();
          for (unsigned i = 0; i != n; ++i)
            {
              unsigned char saved = b[y+i];
              b[y+i] ^= 128;
              int sign = a[x+i] < b[y+i] ? -1 : 1;
              int value = ordered (a+x, b+y, n);
              if (equal (a+x, b+y, n) || !different (a+x, b+y, n)
                  || (value < 0 ? -1 : value > 0) != sign)
                __builtin_abort ();
              b[y+i] = saved;
            }
        }
  if (ac != 16 * 16 * 97 || bc != ac || nc != ac)
    __builtin_abort ();
  return 0;
}
/* { dg-final { scan-rtl-dump "Expanded bounded dynamic equality comparison" "expand" } } */
