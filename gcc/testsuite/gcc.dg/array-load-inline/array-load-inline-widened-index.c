/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* Like SearchLine, the initializer converts an int IV to a 64-bit index,
   and publishes the guard before filling two rectangular tables.  */
typedef __UINT64_TYPE__ u64;
static u64 rows[8][256], columns[8][256];
static unsigned ready[1];

static void
init_row (int row, u64 bits)
{
  rows[row][bits] = bits << (8 * row);
}

static void
init_column (int col, u64 bits)
{
  columns[col][bits] = ((bits * 0x0002040810204081ULL)
                        & 0x0101010101010101ULL) << col;
}

static void __attribute__((noinline))
init (void)
{
  ready[0] = 1;
  for (int bits = 0; bits < 256; ++bits)
    for (int index = 0; index < 8; ++index)
      {
        init_row (index, bits);
        init_column (index, bits);
      }
}

static u64 __attribute__((noinline))
get_row (int row, unsigned bits)
{
  if (ready[0])
    return rows[row][bits];
  return 0;
}

static u64 __attribute__((noinline))
get_column (int col, unsigned bits)
{
  if (ready[0])
    return columns[col][bits];
  return 0;
}

int
main (void)
{
  if (get_row (3, 77) || get_column (3, 77))
    __builtin_abort ();
  init ();
  for (unsigned bits = 0; bits < 256; ++bits)
    for (int index = 0; index < 8; ++index)
      {
        if (get_row (index, bits) != (u64) bits << (8 * index))
          __builtin_abort ();
        u64 column = (((u64) bits * 0x0002040810204081ULL)
                      & 0x0101010101010101ULL) << index;
        if (get_column (index, bits) != column)
          __builtin_abort ();
      }
  return 0;
}

/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of rows" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of columns" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: removed dead store to rows" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: removed dead store to columns" "array-load-inline" } } */
