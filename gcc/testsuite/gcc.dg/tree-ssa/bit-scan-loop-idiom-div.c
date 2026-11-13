/* Disabling the extension preserves recognition of division that range
   propagation has already converted to the original right-shift idiom.  */
/* { dg-do compile } */
/* { dg-require-effective-target clz } */
/* { dg-options "-O3 -fno-bit-scan-loop-idiom -fdump-tree-optimized" } */

int
clz_div (unsigned value)
{
  int count = 32;
  while (value != 0)
    {
      value /= 2;
      count--;
    }
  return count;
}

/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 1 "optimized" } } */
