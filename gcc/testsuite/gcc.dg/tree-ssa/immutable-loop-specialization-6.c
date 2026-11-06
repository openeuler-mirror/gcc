/* { dg-do run } */
/* { dg-options "-O3 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details" } */

/* The start state is selected at run time from two offsets in one object.
   It is a profitability hint, not a fixed trip count.  */
#ifndef MUTABLE
#define QUAL const
#else
#define QUAL
#endif
static QUAL unsigned char a[] = {1, 2, 3, 15, 4, 5, 15};
static QUAL unsigned char b[] = {2, 5, 4, 15, 1, 3, 15};
#ifdef DIFFERENT
static QUAL unsigned char c[] = {3, 1, 2, 15, 5, 4, 15};
#define SECOND c
#else
#define SECOND b
#endif

__attribute__((noinline))
static unsigned
interpret (const unsigned char *program, unsigned value, unsigned select)
{
  if (!program) return value;
  const unsigned char *p;
  if (select & 1)
    {
      __asm__ volatile ("");
      p = program;
    }
  else
    {
      __asm__ volatile ("");
#ifdef DIFFERENT
      p = b + 4;
#else
      p = program + 4;
#endif
    }
  while (*p != 15)
    {
      unsigned op = *p++;
      switch (op)
	{
	case 1: value += 11; break;
	case 2: value ^= 31; break;
	case 3: value *= 7; break;
	case 4: value = (value >> 2) | (value << 30); break;
	case 5: value -= 17; break;
	default: value ^= op;
	}
    }
  return value;
}

__attribute__((noinline, noclone, optimize("O0")))
static unsigned
reference (const unsigned char *program, unsigned value, unsigned select)
{
#ifdef DIFFERENT
  const unsigned char *p = (select & 1) ? program : b + 4;
#else
  const unsigned char *p = program + ((select & 1) ? 0 : 4);
#endif
  while (*p != 15)
    {
      unsigned op = *p++;
      switch (op)
	{
	case 1: value += 11; break;
	case 2: value ^= 31; break;
	case 3: value *= 7; break;
	case 4: value = (value >> 2) | (value << 30); break;
	case 5: value -= 17; break;
	default: value ^= op;
	}
    }
  return value;
}

int main (void)
{
  for (unsigned i = 0; i < 100; ++i)
    {
      unsigned x = i * 42949671U, select = i;
      __asm__ volatile ("" : "+r" (x), "+r" (select));
#ifdef MUTABLE
      a[0] = 1 + i % 5;
      b[0] = 1 + i % 5;
#endif
      if (interpret (a, x, select) + interpret (a, x + 1, select)
	  + interpret (SECOND, x, select) + interpret (SECOND, x + 1, select)
	  != reference (a, x, select) + reference (a, x + 1, select)
	     + reference (SECOND, x, select) + reference (SECOND, x + 1, select))
	__builtin_abort ();
    }
  return 0;
}
/* { dg-final { scan-tree-dump "Pointer PHI shares one constant-address object" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump "Bounded immutable state peeling" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump "Peeled loop" "immutable-unroll1" } } */
