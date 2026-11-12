/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -mlzcnt -fdump-tree-forwprop1-details" } */

/* With the option disabled, retain GCC 14's existing CTZ table folding,
   but do not enable the backported CLZ table folding.  */

unsigned int
debruijn_ctz (unsigned int x)
{
  static const unsigned int table[32] = {
    0, 1, 28, 2, 29, 14, 24, 3,
    30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7,
    26, 12, 18, 6, 11, 5, 10, 9
  };
  return table[((x & -x) * 0x077cb531U) >> 27];
}

unsigned int
debruijn_clz (unsigned int x)
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

/* { dg-final { scan-tree-dump-times {= \.CTZ} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not {\.CLZ|__builtin_clz} "forwprop1" } } */
