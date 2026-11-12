/* { dg-do run } */
/* { dg-options "-O3 -fipa-cp-clone -fpeel-loops -fipa-value-specialization --param ipa-cp-eval-threshold=0 --param ipa-value-specialization-peel-growth-budget=4096 --param ipa-value-specialization-peel-growth-percent=0" } */

#include "readonly-peel-growth-nested.c"

static volatile unsigned calls;

__attribute__((noipa))
unsigned
observe_byte (unsigned a, unsigned b)
{
  ++calls;
  return (a * 17) ^ b;
}

__attribute__((noipa, optimize ("O0")))
static unsigned
reference (const unsigned char *a, const unsigned char *b, unsigned x)
{
  for (const unsigned char *p = a; *p; ++p)
    for (const unsigned char *q = b; *q; ++q)
      {
        x += observe_byte (*p, *q);
        x ^= observe_byte (*q, *p);
        x += observe_byte (*p + *q, x);
      }
  return x;
}

int
main (void)
{
  static const unsigned char a[64]
    = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz?";
  static const unsigned char b[64]
    = "?zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA9876543210";
  for (unsigned x = 0; x < 7; ++x)
    {
      calls = 0;
      unsigned actual = evaluate_pair (x);
      unsigned actual_calls = calls;
      calls = 0;
      unsigned expected = reference (a, b, x) + reference (b, a, x + 1);
      if (actual != expected || actual_calls != calls
          || calls != 2 * 63 * 63 * 3)
        __builtin_abort ();
    }
  return 0;
}
