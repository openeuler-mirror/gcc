#include "value-specialization-function-options.h"
__attribute__((noipa, optimize ("O0"))) static int expected (int x)
{
  static const unsigned char *const patterns[] = {(const unsigned char *) "a%1|b", (const unsigned char *) "c%2|d", (const unsigned char *) "e%3|f", (const unsigned char *) "g%4|h", (const unsigned char *) "i%5|j", (const unsigned char *) "k%6|l", (const unsigned char *) "m%7|n", (const unsigned char *) "o%8|p", 0, (const unsigned char *) "q%9|r", (const unsigned char *) "s%0|t", (const unsigned char *) "u%1|v", (const unsigned char *) "w%2|x", (const unsigned char *) "y%3|z", (const unsigned char *) "A%4|B", (const unsigned char *) "C%5|D", (const unsigned char *) "E%6|F"};
  int sum = 0;
  for (unsigned k = 0; k != 200; ++k)
    for (unsigned j = 0; j != sizeof (patterns) / sizeof (*patterns); ++j)
      {
        const unsigned char *p = patterns[j];
        int value = 0;
        if (!p) { sum += x; continue; }
        while (*p)
          {
            if (*p == '%') value += x;
            else if (*p == '|') value ^= x;
            else value += *p;
            ++p;
          }
        sum += value;
      }
  return sum;
}
int main (void)
{
  for (int x = 0; x != 8; ++x)
    if (test (x) != expected (x)) __builtin_abort ();
  return 0;
}
