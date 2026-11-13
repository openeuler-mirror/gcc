/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fno-peel-loops -fdump-ipa-cp-details" } */

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
  if (!pattern)
    return value;
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
      result += interpret ((const unsigned char *) "a%1|b", x);
      result += interpret ((const unsigned char *) "c%2|d", x);
      result += interpret ((const unsigned char *) "e%3|f", x);
      result += interpret ((const unsigned char *) "g%4|h", x);
      result += interpret ((const unsigned char *) "i%5|j", x);
      result += interpret ((const unsigned char *) "k%6|l", x);
      result += interpret ((const unsigned char *) "m%7|n", x);
      result += interpret ((const unsigned char *) "o%8|p", x);
      result += interpret ((const unsigned char *) 0, x);
      result += interpret ((const unsigned char *) "q%9|r", x);
      result += interpret ((const unsigned char *) "s%0|t", x);
      result += interpret ((const unsigned char *) "u%1|v", x);
      result += interpret ((const unsigned char *) "w%2|x", x);
      result += interpret ((const unsigned char *) "y%3|z", x);
      result += interpret ((const unsigned char *) "A%4|B", x);
      result += interpret ((const unsigned char *) "C%5|D", x);
      result += interpret ((const unsigned char *) "E%6|F", x);
    }
  return result;
}

/* With the new option left at its default, the ordinary value-list limit and
   therefore GCC's pre-existing IPA-CP behavior are preserved.  */
/* { dg-final { scan-ipa-dump-not "Creating a specialized node of interpret" "cp" } } */
