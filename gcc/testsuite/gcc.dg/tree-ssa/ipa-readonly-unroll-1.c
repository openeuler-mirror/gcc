/* { dg-do compile } */
/* { dg-options "-O3 -fchecking=2 -fipa-cp -fipa-cp-clone -fipa-value-specialization -fdump-tree-cunroll-details" } */

__attribute__((noinline, noclone))
static int
opaque (int x)
{
  __asm__ ("" : "+r" (x));
  return x;
}

__attribute__((noinline))
static int
interpret (const unsigned char *pattern, int value)
{
  int result = 0;
  #pragma GCC unroll 1
  while (*pattern)
    {
      if (*pattern == '%')
	result += value;
      else if (*pattern == '|')
	result ^= value;
      else
	result += *pattern;
      ++pattern;
    }
  return result;
}

int
test (int x)
{
  int result = 0;
  x = opaque (x);
  for (int i = 0; i < 200; ++i)
    {
      result += interpret ((const unsigned char *) "alpha%1|beta", x);
      result += interpret ((const unsigned char *) "gamma%2|delta", x);
    }
  return result;
}

/* An immutable-value hint must not override the user unroll factor.  */
/* { dg-final { scan-tree-dump "Not peeling: user didn't want it peeled" "cunroll" } } */
/* { dg-final { scan-tree-dump-not "Peeled IPA readonly-value loop" "cunroll" } } */
