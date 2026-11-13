/* The original shift-based recognition remains active with the option off.  */
/* { dg-do compile } */
/* { dg-require-effective-target clz } */
/* { dg-require-effective-target ctz } */
/* { dg-options "-O3 -fno-bit-scan-loop-idiom -fdump-tree-optimized" } */

unsigned
ctz_shift (unsigned value)
{
  int count = 32;
  while (value != 0)
    {
      count--;
      value <<= 1;
    }
  return count;
}

int
clz_shift (unsigned value)
{
  int count = 32;
  while (value != 0)
    {
      value >>= 1;
      count--;
    }
  return count;
}

/* { dg-final { scan-tree-dump-times "__builtin_ctz|\\.CTZ" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 1 "optimized" } } */
