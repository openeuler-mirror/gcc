/* { dg-do run } */
/* { dg-options "-O3 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-dom-details" } */
/* Data-dependent program state and observable side effects must survive
   repeated state peeling and path simplification.  */
/* A generic byte-program interpreter.  The helper updates local state,
   including data-dependent state and arithmetic; no application grammar
   or identifier participates in recognizing this program.  */
static volatile unsigned visits;

static unsigned
step (const unsigned char **state, unsigned x)
{
  ++visits;
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
    case 13: *state += x & 1; return x ^ 9;
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

#ifndef IMMUTABLE_PROGRAM_A
#define IMMUTABLE_PROGRAM_A "\001\015\003\004\002\001\015\002\003\004"
#endif
static const unsigned char a[] = IMMUTABLE_PROGRAM_A;
static const unsigned char b[] = "\006\007\011\012";
static const unsigned char c[] = "\007\010\013\014\005";

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
      visits = 0;
      unsigned actual = interpret (a, x) + interpret (a, x + 1)
			+ interpret (b, x) + interpret (b, x + 1)
			+ interpret (c, x) + interpret (c, x + 1);
      unsigned actual_visits = visits;
      visits = 0;
      unsigned expected = reference (a, x) + reference (a, x + 1)
			  + reference (b, x) + reference (b, x + 1)
			  + reference (c, x) + reference (c, x + 1);
      if (actual != expected || actual_visits != visits)
	__builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump "Function interpret" "immutable-dom1" } } */
/* { dg-final { scan-tree-dump "Function interpret" "immutable-dom2" } } */
