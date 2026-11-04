/* A helper above the original edge limit needs the opt-in reserve. */
/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization  --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 --param ipa-cp-eval-threshold=0 -fdump-ipa-cp-details -fdump-ipa-inline-details  -fchecking=2 -fipa-value-specialization-helper-expansion" } */
/* { dg-final { scan-ipa-dump "Inlining inherited immutable program helper" "inline" } } */
#ifndef READONLY
#define READONLY const
#endif
/* A generic byte-program interpreter.  The helper updates local state,
   including data-dependent state and arithmetic; no application grammar
   or identifier participates in recognizing this program.  */
static unsigned
step (const unsigned char **state, unsigned x)
{
  unsigned op = *(*state)++;
  switch (op)
    {
#define OP(N) case (N): x += (N); x ^= x >> ((N) % 7 + 1); \
  x *= (N) + 3; x ^= x << ((N) % 11 + 1); x ^= x >> 13; \
  x *= 7919; x += x << 17; x ^= x >> 7; x *= 104729; \
  return x + (N) * 3;
#define OP8(N) OP((N) + 1) OP((N) + 2) OP((N) + 3) OP((N) + 4) \
  OP((N) + 5) OP((N) + 6) OP((N) + 7) OP((N) + 8)
      OP8(0)
      OP8(8)
      OP8(16)
      OP8(24)
      OP8(32)
      OP8(40)
      OP8(48)
      OP8(56)
      OP8(64)
      OP8(72)
      OP8(80)
      OP8(88)
      OP8(96)
      OP8(104)
      OP8(112)
      OP8(120)
#undef OP8
#undef OP
    default: return x;
    }
}

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
