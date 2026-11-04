/* Verify default-off behavior and signed-division rejection.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

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
