/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization -fipa-value-specialization-loop --param early-inlining-insns=0 --param ipa-cp-eval-threshold=10000000 -fdump-ipa-cp-details" } */
/* A generic byte-program interpreter.  The helper updates local state,
   including data-dependent state and arithmetic; no application grammar
   or identifier participates in recognizing this program.  */
static unsigned
step (const unsigned char **state, unsigned x)
{
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

#ifndef INTERP_ATTRIBUTES
#define INTERP_ATTRIBUTES __attribute__((noinline))
#endif
INTERP_ATTRIBUTES
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

#ifdef MUTABLE_PROGRAM
#define PROGRAM_READONLY
#else
#define PROGRAM_READONLY const
#endif

static PROGRAM_READONLY unsigned char a[] = "\001\003\004\002";
static PROGRAM_READONLY unsigned char b[] = "\006\007\011\012";
static PROGRAM_READONLY unsigned char c[] = "\007\010\013\014\005";

__attribute__((noinline, noclone))
static unsigned
opaque (unsigned x)
{
  __asm__ volatile ("" : "+r" (x));
  return x;
}

__attribute__((noinline))
static unsigned
evaluate (unsigned x)
{
  return interpret (a, x) + interpret (b, x) + interpret (c, x);
}

#ifdef TWO_WRAPPERS
__attribute__((noinline))
static unsigned
forward (unsigned x)
{
  return evaluate (x);
}
#define EVALUATE_ENTRY forward
#else
#define EVALUATE_ENTRY evaluate
#endif

int main (void)
{
#ifndef EXECUTE_ONCE
  for (unsigned i = 0; i != 100; ++i)
    {
#else
    {
      unsigned i = 37;
#endif
      unsigned x = opaque (i * 42949671U);
#ifdef MUTABLE_PROGRAM
      a[0] = (x & 1) + 1;
      b[0] = (x & 3) + 1;
      c[0] = (x & 7) + 1;
#endif
#ifdef DIRECT_LOOP_CALLS
      unsigned actual = interpret (a, x) + interpret (b, x) + interpret (c, x);
#else
      unsigned actual = EVALUATE_ENTRY (x);
#endif
      unsigned expected = reference (a, x)
			  + reference (b, x)
			  + reference (c, x) ;
      if (actual != expected)
	__builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-ipa-dump "Budgeted loop value clone:" "cp" } } */
