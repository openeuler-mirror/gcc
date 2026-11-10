/* { dg-do run } */
/* { dg-options "-O2 -ftree-constant-byte-range" } */
#include <stddef.h>

__attribute__((noipa)) int walk (unsigned n)
{
  const char *p = "abcxyz";
  int sum = 0;
  while (n-- && *p)
    {
      if (*p == '%' || *p == '!')
        return -1;
      sum += (unsigned char) *p++;
    }
  return sum;
}
__attribute__((noipa)) int high (unsigned n, int sign)
{
  const char *p = "\001\177\200\377" + n;
  return sign ? *(const signed char *) p : *(const unsigned char *) p;
}
__attribute__((noipa)) int search (unsigned n, int c)
{
  const char *p = "abcxyz";
  while (n-- && *p)
    ++p;
  const char *q = __builtin_strchr (p, c);
  return q ? (int) (q - p) : -1;
}
__attribute__((noipa)) int absent (unsigned n)
{
  const char *p = "abcxyz";
  while (n-- && *p)
    ++p;
  return __builtin_strchr (p, '@') == 0;
}
int main (void)
{
  static const unsigned char bytes[] = {1,127,128,255,0};
  const char *s = "abcxyz";
  int sum = 0;
  for (unsigned n = 0; n != 12; ++n)
    {
      if (walk (n) != sum || !absent (n))
        __builtin_abort ();
      unsigned off = n < 6 ? n : 6;
      for (int c = 0; c != 256; ++c)
        {
          int expect = -1;
          for (unsigned i = off; i != 7; ++i)
            if ((unsigned char) s[i] == c)
              { expect = (int) (i-off); break; }
          if (search (n,c) != expect)
            __builtin_abort ();
        }
      if (n < 6) sum += (unsigned char) s[n];
    }
  for (unsigned n = 0; n != 5; ++n)
    if (high (n,0) != bytes[n] || high (n,1) != (signed char) bytes[n])
      __builtin_abort ();
  return 0;
}
