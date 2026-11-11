/* PR tree-optimization/114760, adapted from pr114760-2.c.
   Signed division truncates towards zero; it is not a logical right shift.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fbit-scan-loop-idiom -fdump-tree-optimized" } */

int
signed_div (int value)
{
  int count = 32;
  while (value != 0)
    {
      value /= 2;
      count--;
    }
  return count;
}

/* { dg-final { scan-tree-dump-not "__builtin_ctz|\\.CTZ|__builtin_clz|\\.CLZ" "optimized" } } */
