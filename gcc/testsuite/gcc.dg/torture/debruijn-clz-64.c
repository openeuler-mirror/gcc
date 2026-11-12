/* { dg-do run } */
/* { dg-options "-fdebruijn-clz" } */

/* Check the result of each 64-bit table idiom, including the zero
   result and the top bit that used to require a shift by 64 in the
   validator.  The MSB-isolation table has a different zero result.  */
typedef __UINT64_TYPE__ U64;

U64 __attribute__((noipa))
ctz_table64 (U64 x)
{
  static const unsigned char table[64] = {
    0, 1, 48, 2, 57, 49, 28, 3,
    61, 58, 50, 42, 38, 29, 17, 4,
    62, 55, 59, 36, 53, 51, 43, 22,
    45, 39, 33, 30, 24, 18, 12, 5,
    63, 47, 56, 27, 60, 41, 37, 16,
    54, 35, 52, 21, 44, 32, 23, 11,
    46, 26, 40, 15, 34, 20, 31, 10,
    25, 14, 19, 9, 13, 8, 7, 6
  };
  return table[((x & -x) * 0x03f79d71b4cb0a89ULL) >> 58];
}

U64 __attribute__((noipa))
msb_table64 (U64 x)
{
  static const unsigned int table[64] = {
    0, 47, 1, 56, 48, 27, 2, 60,
    57, 49, 41, 37, 28, 16, 3, 61,
    54, 58, 35, 52, 50, 42, 21, 44,
    38, 32, 29, 23, 17, 11, 4, 62,
    46, 55, 26, 59, 40, 36, 15, 53,
    34, 51, 20, 43, 31, 22, 10, 45,
    25, 39, 14, 33, 19, 30, 9, 24,
    13, 18, 8, 12, 7, 6, 5, 63
  };
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x |= x >> 32;
  return table[(x * 0x03f79d71b4cb0a89ULL) >> 58];
}

U64 __attribute__((noipa))
msb_isolated64 (U64 x)
{
  static const U64 table[64] = {
    63, 0, 58, 1, 59, 47, 53, 2,
    60, 39, 48, 27, 54, 33, 42, 3,
    61, 51, 37, 40, 49, 18, 28, 20,
    55, 30, 34, 11, 43, 14, 22, 4,
    62, 57, 46, 52, 38, 26, 32, 41,
    50, 36, 17, 19, 29, 10, 13, 21,
    56, 45, 25, 31, 35, 16, 9, 12,
    44, 24, 15, 8, 23, 7, 6, 5
  };
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x |= x >> 32;
  return table[((x - (x >> 1)) * 0x07edd5e59a4e28c2ULL) >> 58];
}

int
main (void)
{
  if (ctz_table64 (0) != 0 || msb_table64 (0) != 0
      || msb_isolated64 (0) != 63)
    __builtin_abort ();

  for (unsigned int i = 0; i < 64; ++i)
    {
      U64 x = (U64) 1 << i;
      if (ctz_table64 (x) != i || msb_table64 (x) != i
          || msb_isolated64 (x) != i)
        __builtin_abort ();
      if (ctz_table64 (x | (x - 1)) != 0
          || msb_table64 (x | (x - 1)) != i
          || msb_isolated64 (x | (x - 1)) != i)
        __builtin_abort ();
    }

  if (ctz_table64 (~(U64) 0) != 0 || msb_table64 (~(U64) 0) != 63
      || msb_isolated64 (~(U64) 0) != 63)
    __builtin_abort ();
  return 0;
}
