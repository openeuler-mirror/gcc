/* Signed multiply overflow must retain its trapping semantics.  */
/* { dg-do compile } */
/* { dg-options "-O3 -ftrapv -fbit-scan-loop-idiom -fdump-tree-optimized" } */

int
signed_mul (int value)
{
  int count = 0;
  while (value != 0)
    {
      value *= 2;
      count++;
    }
  return count;
}

/* { dg-final { scan-tree-dump-not "__builtin_ctz|\\.CTZ|__builtin_clz|\\.CLZ" "optimized" } } */
