/* { dg-do run } */
/* { dg-options "-fdebruijn-clz" } */

/* Table results for zero need not fit in the int returned by the count-zero
   internal functions.  Do not truncate them when folding a wider table.  */

typedef __UINT32_TYPE__ U32;
typedef unsigned long long U64;

U64 __attribute__((noipa))
table_ctz (U32 x)
{
  static const U64 table[64] = {
    4294967296ULL, 0, 0, 1, 28, 0, 0, 2,
    29, 0, 0, 14, 24, 0, 3, 0,
    30, 0, 0, 22, 20, 0, 15, 0,
    25, 0, 17, 0, 0, 4, 0, 8,
    31, 0, 27, 0, 0, 13, 23, 0,
    0, 21, 19, 0, 0, 16, 0, 7,
    0, 26, 12, 0, 0, 18, 0, 6,
    0, 11, 0, 5, 10, 0, 9, 0
  };
  return table[((x & -x) * 0x077cb531U) >> 26];
}

U64 __attribute__((noipa))
table_msb (U32 x)
{
  static const U64 table[64] = {
    4294967296ULL, 0, 9, 0, 0, 1, 0, 10,
    13, 0, 0, 21, 0, 2, 29, 0,
    11, 0, 0, 14, 16, 0, 0, 18,
    0, 22, 0, 25, 0, 3, 30, 0,
    8, 0, 0, 12, 20, 0, 28, 0,
    0, 15, 17, 0, 24, 0, 0, 7,
    0, 19, 27, 0, 0, 23, 6, 0,
    26, 0, 5, 0, 4, 0, 31, 0
  };
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return table[(x * 0x07c4acddU) >> 26];
}

int
main (void)
{
  if (table_ctz (0) != 4294967296ULL || table_msb (0) != 4294967296ULL)
    __builtin_abort ();
  for (unsigned int i = 0; i < 32; ++i)
    {
      U32 x = (U32) 1 << i;
      if (table_ctz (x) != i || table_msb (x) != i)
        __builtin_abort ();
    }
  if (table_ctz (~(U32) 0) != 0 || table_msb (~(U32) 0) != 31)
    __builtin_abort ();
  return 0;
}
