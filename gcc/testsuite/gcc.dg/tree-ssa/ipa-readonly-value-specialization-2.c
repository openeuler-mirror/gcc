/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-tree-cunroll-details" } */

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

/* The option is default-off, so the IPA-specific loop transform must not run.  */
/* { dg-final { scan-tree-dump-not "Peeling IPA readonly-value loop" "cunroll" } } */
