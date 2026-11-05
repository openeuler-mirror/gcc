/* Check defined zero results and bit-position boundaries at run time.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-ch -fno-tree-vrp -fbit-scan-loop-idiom" } */

unsigned ctz_mul (unsigned) __attribute__((noipa));
unsigned ctz_add (unsigned) __attribute__((noipa));
int clz_div (unsigned) __attribute__((noipa));
unsigned ctz_div_bit (unsigned) __attribute__((noipa));
unsigned clz_mul_sign (unsigned) __attribute__((noipa));

#include "bit-scan-loop-idiom-1.c"
#include "bit-scan-loop-idiom-bit.c"

static void
check (unsigned value)
{
  unsigned expected_ctz = 32;
  int expected_clz = 32;
  if (value != 0)
    {
      int width = sizeof (unsigned) * __CHAR_BIT__;
      expected_ctz = 32 - width + __builtin_ctz (value);
      expected_clz = 32 - width + __builtin_clz (value);
    }
  if (ctz_mul (value) != expected_ctz
      || ctz_add (value) != expected_ctz
      || clz_div (value) != expected_clz)
    __builtin_abort ();
  unsigned width = sizeof (unsigned) * __CHAR_BIT__;
  unsigned bit_ctz = value ? __builtin_ctz (value) : width;
  unsigned bit_clz = value ? __builtin_clz (value) : width;
  if (ctz_div_bit (value) != bit_ctz
      || clz_mul_sign (value) != bit_clz)
    __builtin_abort ();
}

int
main (void)
{
  check (0);
  check (~0U);
  for (unsigned i = 0; i < sizeof (unsigned) * __CHAR_BIT__; ++i)
    {
      unsigned bit = (unsigned) 1 << i;
      check (bit);
      check (bit - 1);
      check (bit | (bit - 1));
    }
  unsigned value = 1;
  for (unsigned i = 0; i < 256; ++i)
    {
      value = value * 1664525U + 1013904223U;
      check (value);
    }
  return 0;
}
