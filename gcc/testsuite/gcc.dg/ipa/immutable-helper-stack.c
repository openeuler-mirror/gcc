/* The immutable helper hint must retain ordinary stack-frame limits.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fchecking=2 -fipa-value-specialization -fno-ipa-value-specialization-helper-expansion --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 --param ipa-cp-eval-threshold=0 -fdump-ipa-inline-details -fstack-usage" } */
#ifndef READONLY
#define READONLY const
#endif
/* A generic byte-program interpreter.  The helper updates local state,
   including data-dependent state and arithmetic; no application grammar
   or identifier participates in recognizing this program.  */
static unsigned
step (const unsigned char **state, unsigned x)
{
  unsigned char scratch[200000];
  /* Keep the complete frame materialized, including at inline estimates.  */
  __asm__ volatile ("" : : "r" (scratch) : "memory");
  unsigned op = *(*state)++;
  switch (op)
    {
    case 1: return x + 3;
    case 2: return x ^ 19;
    case 3: return x * 7;
    case 4: return (x >> 2) | (x << 30);
    case 5: return x - 31;
    case 6: return x + *(*state)++;
    case 7: return x * x + 1;
    case 8: return x ^ (x >> 3);
    case 9: return (x & 255) + 17;
    case 10: return x / 3;
    case 11: return x % 17;
    case 12: return x + (x << 4);
    default: return x;
    }
}

__attribute__((noinline))
static unsigned
interpret (const unsigned char *p, unsigned x)
{
  if (!p || !*p)
    return x;
  while (*p)
    {
      const unsigned char *next = p;
      x = step (&next, x);
      p = next;
    }
  return x;
}

__attribute__((noinline, noclone, optimize("O0")))
static unsigned
reference (const unsigned char *p, unsigned x)
{
  while (*p)
    x = step (&p, x);
  return x;
}

static READONLY unsigned char a[] = "\001\003\004\002";
static READONLY unsigned char b[] = "\006\007\011\012";
static READONLY unsigned char c[] = "\007\010\013\014\005";

__attribute__((noinline, noclone))
static unsigned
opaque (unsigned x)
{
  __asm__ volatile ("" : "+r" (x));
  return x;
}

int main (void)
{
  for (unsigned i = 0; i != 100; ++i)
    {
      unsigned x = opaque (i * 42949671U);
#ifdef MUTABLE_PROGRAM
      a[0] = (x & 1) + 1;
      b[0] = (x & 7) + 1;
      c[0] = (x & 3) + 1;
#endif
      unsigned actual = interpret (a, x)
			+ interpret (b, x)
			+ interpret (c, x);
      unsigned expected = reference (a, x)
			  + reference (b, x)
			  + reference (c, x);
      if (actual != expected)
	__builtin_abort ();
    }
  return 0;
}
/* { dg-final { scan-ipa-dump "large-stack-frame-growth limit reached" "inline" } } */
/* { dg-final { scan-ipa-dump-not "Inlining bounded immutable program helper" "inline" } } */
