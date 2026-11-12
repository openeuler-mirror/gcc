static const unsigned char first[] = "a|b";
static const unsigned char second[] = "c%d";
static const unsigned char walk[] = "abcdefghi";
__attribute__((noinline)) static unsigned interpret (const unsigned char *p, unsigned x)
{
  unsigned result = 0;
  if (!p) return x;
  while (*p) { result += *p == '%' ? x : *p; ++p; }
  return result;
}
__attribute__((noinline, noclone)) unsigned context_caller (unsigned x)
{
  unsigned result = interpret (first, x) + interpret (second, x);
  for (const unsigned char *p = walk; *p; ++p)
    { result += *p + x; __asm__ volatile ("" : : : "memory"); }
  return result;
}
int main (void)
{
  for (unsigned x = 0; x != 8; ++x)
    {
      unsigned expected = 'a' + '|' + 'b' + 'c' + x + 'd';
      for (unsigned i = 0; walk[i]; ++i) expected += walk[i] + x;
      if (context_caller (x) != expected) __builtin_abort ();
    }
  return 0;
}
