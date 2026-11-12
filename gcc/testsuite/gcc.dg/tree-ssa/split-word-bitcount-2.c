/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fno-debruijn-clz -fdump-tree-optimized" } */

typedef unsigned int U32;
typedef unsigned long long U64;

static inline unsigned
ctz32_fallback (U32 val)
{
  static const U32 table[32] = {
    0, 1, 28, 2, 29, 14, 24, 3,
    30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7,
    26, 12, 18, 6, 11, 5, 10, 9
  };
  return table[((U32) ((val & ((~val) + 1)) * 0x077CB531U)) >> 27];
}

static inline unsigned
clz32_fallback (U32 val)
{
  static const U32 table[32] = {
    0, 9, 1, 10, 13, 21, 2, 29,
    11, 14, 16, 18, 22, 25, 3, 30,
    8, 12, 20, 28, 15, 17, 24, 7,
    19, 27, 23, 6, 26, 5, 4, 31
  };
  val |= val >> 1;
  val |= val >> 2;
  val |= val >> 4;
  val |= val >> 8;
  val |= val >> 16;
  return 31 - table[(val * 0x07C4ACDDU) >> 27];
}

__attribute__((noinline, noclone))
unsigned
ctz32 (U32 val)
{
  return ctz32_fallback (val);
}

__attribute__((noinline, noclone))
unsigned
clz32 (U32 val)
{
  return clz32_fallback (val);
}

__attribute__((noinline, noclone))
unsigned
ctz64 (U64 val)
{
  U32 high = (U32) (val >> 32);
  U32 low = (U32) val;
  if (low == 0)
    return 32 + ctz32_fallback (high);
  return ctz32_fallback (low);
}

__attribute__((noinline, noclone))
unsigned
clz64 (U64 val)
{
  U32 high = (U32) (val >> 32);
  U32 low = (U32) val;
  if (high == 0)
    return 32 + clz32_fallback (low);
  return clz32_fallback (high);
}

/* Keep the original CTZ table recognition, but disable both the
   backported CLZ table recognition and the split-word replacements.  */
/* { dg-final { scan-tree-dump-times {= \.CTZ} 3 "optimized" } } */
/* { dg-final { scan-tree-dump-not {\.CLZ|__builtin_clz} "optimized" } } */
/* { dg-final { scan-tree-dump {& 31} "optimized" } } */
