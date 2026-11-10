/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* Initialization completion must use both loops, including the converted
   second index, even when initialization and the load share a function.  */
typedef __UINT64_TYPE__ u64;
static u64 tbl[8][256];

static u64 __attribute__((noinline))
get (unsigned row, unsigned bits)
{
  for (int r = 0; r < 8; ++r)
    for (int b = 0; b < 256; ++b)
      tbl[r][(u64) b] = (u64) b << (8 * r);
  return tbl[row][bits];
}

int
main (void)
{
  if (get (7, 255) != (u64) 255 << 56
      || get (0, 127) != 127)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: removed dead store to tbl" "array-load-inline" } } */
