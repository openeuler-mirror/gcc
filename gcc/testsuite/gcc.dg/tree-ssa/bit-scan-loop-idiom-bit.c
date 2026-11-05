/* Exercise the bit-test and signed-comparison entry, rather than the
   nonzero-test entry used by bit-scan-loop-idiom-1.c.  */
/* { dg-do compile } */
/* { dg-require-effective-target clz } */
/* { dg-require-effective-target ctz } */
/* Keep the tested exit in the loop header, as in clz-int.c and ctz-int.c.
   Disable VRP so division still reaches niter as division, not a shift.  */
/* { dg-options "-O3 -fno-tree-ch -fno-tree-vrp -fbit-scan-loop-idiom -fno-ipa-icf -fdump-tree-optimized" } */

unsigned
ctz_div_bit (unsigned value)
{
  unsigned count = 0;
  if (!value)
    return sizeof (unsigned) * __CHAR_BIT__;
  while (!(value & 1))
    {
      value /= 2;
      ++count;
    }
  return count;
}

unsigned
clz_mul_sign (unsigned value)
{
  unsigned count = 0;
  if (!value)
    return sizeof (unsigned) * __CHAR_BIT__;
  while ((int) value >= 0)
    {
      value *= 2;
      ++count;
    }
  return count;
}

/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ctz|\\.CTZ" 1 "optimized" } } */
