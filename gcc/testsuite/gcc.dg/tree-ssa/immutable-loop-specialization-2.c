/* { dg-do run } */
/* { dg-options "-O3 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details" } */

/* Nested state loops become eligible only after an enclosing constant
   program state has been exposed and propagated.  */
static unsigned
step (const unsigned char **state, unsigned value)
{
  const unsigned char *p = *state;
  while (*p == 0)
    ++p;
  if (*p == 15)
    {
      *state = p;
      return value;
    }
  unsigned op = *p++;
  *state = p;
  switch (op)
    {
    case 1: return value + 11;
    case 2: return value ^ 31;
    case 3: return value * 7;
    case 4: return (value >> 2) | (value << 30);
    case 5: return value - 17;
    default: return value;
    }
}

__attribute__((noinline))
static unsigned
interpret (const unsigned char *p, unsigned value)
{
  if (!p) return value;
  while (*p != 15)
    {
      const unsigned char *next = p;
      value = step (&next, value);
      p = next;
    }
  return value;
}

__attribute__((noinline, noclone, optimize("O0")))
static unsigned
reference (const unsigned char *p, unsigned value)
{
  while (*p != 15)
    value = step (&p, value);
  return value;
}

static const unsigned char a[] = {1, 0, 0, 2, 0, 3, 0, 15};
static const unsigned char b[] = {0, 4, 0, 5, 0, 0, 15};

int main (void)
{
  for (unsigned i = 0; i != 100; ++i)
    {
      unsigned x = i * 42949671U;
      __asm__ volatile ("" : "+r" (x));
      if (interpret (a, x) + interpret (a, x + 1)
	  + interpret (b, x) + interpret (b, x + 1)
	  != reference (a, x) + reference (a, x + 1)
	     + reference (b, x) + reference (b, x + 1))
	__builtin_abort ();
    }
}
/* { dg-final { scan-tree-dump "Bounded immutable state peeling" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump "Bounded immutable state peeling" "immutable-unroll2" } } */
/* { dg-final { scan-tree-dump "Peeled loop" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump "Peeled loop" "immutable-unroll2" } } */
