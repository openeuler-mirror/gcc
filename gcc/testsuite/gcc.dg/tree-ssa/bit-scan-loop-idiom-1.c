/* Verify opt-in recognition of multiply/add/divide bit-scan loops.  */
/* { dg-do compile } */
/* { dg-require-effective-target clz } */
/* { dg-require-effective-target ctz } */
/* Disable ICF so the multiply and add functions remain separate bodies.  */
/* { dg-options "-O3 -fbit-scan-loop-idiom -fno-ipa-icf -fdump-tree-optimized" } */

unsigned
ctz_mul (unsigned value)
{
  int count = 32;
  while (value != 0)
    {
      count--;
      value *= 2;
    }
  return count;
}

unsigned
ctz_add (unsigned value)
{
  int count = 32;
  while (value != 0)
    {
      count--;
      value += value;
    }
  return count;
}

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

/* { dg-final { scan-tree-dump-times "__builtin_ctz|\\.CTZ" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 1 "optimized" } } */
