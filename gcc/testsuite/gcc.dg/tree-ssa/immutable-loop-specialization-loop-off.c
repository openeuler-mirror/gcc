/* { dg-do compile } */
/* { dg-options "-O3 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fno-tree-loop-optimize -fdump-tree-optimized" } */

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

static const unsigned char a[] = "\001\003\004\002", b[] = "\006\007\011\012";
static const unsigned char tab[] = "\001\002\003\004\005\001\002\003\004";
unsigned sink[2208];
__attribute__((noinline)) unsigned driver (unsigned x)
{
  unsigned r = interpret (a, x) + interpret (a, x + 1) + interpret (b, x) + interpret (b, x + 1);
  for (const unsigned char *q = tab; *q; ) x = x * 3 + *q++;
  sink[0] = x;
  return r + x;
}

/* The ordinary optimized dump exists even when the optional pipeline is
   disabled.  The constant byte traversal must retain its loop.  */
/* { dg-final { scan-tree-dump "q_.*PHI" "optimized" } } */
