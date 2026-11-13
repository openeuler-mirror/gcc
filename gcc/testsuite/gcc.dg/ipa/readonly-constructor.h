/* Use a real CONSTRUCTOR large enough for separate LTO streaming.  */
static const unsigned a[16] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0 };
static const unsigned b[16] = { 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 };

__attribute__((noinline))
static unsigned
interpret (const unsigned *pattern, unsigned value)
{
  unsigned result = value;
  if (!pattern)
    return result;
  while (*pattern)
    {
      if (*pattern == 3)
        result += value;
      else if (*pattern & 1)
        result = result * 3 + *pattern;
      else
        result ^= value + *pattern;
      ++pattern;
    }
  return result;
}

__attribute__((noipa, optimize("O0")))
static unsigned
reference (const unsigned *pattern, unsigned value)
{
  unsigned result = value;
  for (unsigned i = 0; pattern[i] != 0; ++i)
    {
      unsigned op = pattern[i];
      if (op == 3)
        result += value;
      else if (op & 1)
        result = result * 3 + op;
      else
        result ^= value + op;
    }
  return result;
}

__attribute__((noipa))
static unsigned
opaque (unsigned x)
{
  __asm__ ("" : "+r" (x));
  return x;
}

int
main (void)
{
  for (unsigned i = 0; i != 16; ++i)
    {
      unsigned x = opaque (i * 42949671U);
      unsigned actual = interpret (a, x) + interpret (b, x + 1);
      unsigned expected = reference (a, x) + reference (b, x + 1);
      if (actual != expected)
        __builtin_abort ();
    }
  return 0;
}
