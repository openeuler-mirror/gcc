/* Verify default-off behavior for the new multiply and add forms.
   Unsigned division can become a right shift before loop analysis and is
   tested separately in bit-scan-loop-idiom-div.c.  */
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

/* { dg-final { scan-tree-dump-not "__builtin_ctz|\\.CTZ|__builtin_clz|\\.CLZ" "optimized" } } */
