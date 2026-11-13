/* { dg-final { scan-ipa-dump-not "Budgeted loop value clone:" "cp" } } */
/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-value-specialization-loop -fipa-value-specialization --param early-inlining-insns=0 --param ipa-cp-eval-threshold=10000000 -fdump-ipa-cp-details" } */

static unsigned step (const unsigned char **p, unsigned x)
{
  switch (*(*p)++)
    {
    case 1: return x + 3;
    case 2: return x ^ 19;
    case 3: return x * 7;
    case 4: return (x >> 2) | (x << 30);
    case 5: return x - 31;
    case 6: return x + *(*p)++;
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
static unsigned interpret (const unsigned char *p, unsigned x)
{
  if (!p || !*p) return x;
  while (*p) x = step (&p,x);
  return x;
}
__attribute__((noinline,noclone,optimize("O0")))
static unsigned reference (const unsigned char *p, unsigned x)
{
  while (*p) x = step (&p,x);
  return x;
}
#ifdef MUTABLE_PROGRAM
#define READONLY
#else
#define READONLY const
#endif
static READONLY unsigned char a[] = "\001\003\004\002";
static READONLY unsigned char b[] = "\007\010\013\014\005";
__attribute__((noinline,noclone))
static unsigned opaque (unsigned x)
{ __asm__ volatile ("" : "+r" (x)); return x; }

__attribute__((noinline,noclone))
static unsigned evaluate (unsigned depth, unsigned x)
{
  unsigned result = 0;
  if (depth)
    result = evaluate (depth-1,x+1) ^ evaluate (depth-1,x+7);
#ifndef NO_LOOP
  for (unsigned i = 0; i != (opaque (x) & 7) + 8; ++i)
#else
  { unsigned i = 0;
#endif
    {
      result += interpret (a,x+i) + interpret (b,x+i);
    }
#ifdef NO_LOOP
  }
#endif
  return result;
}
__attribute__((noinline,noclone,optimize("O0")))
static unsigned expected (unsigned depth, unsigned x)
{
  unsigned result = 0;
  if (depth)
    result = expected (depth-1,x+1) ^ expected (depth-1,x+7);
#ifndef NO_LOOP
  for (unsigned i = 0; i != (x & 7) + 8; ++i)
#else
  { unsigned i = 0;
#endif
    {
      result += reference (a,x+i) + reference (b,x+i);
    }
#ifdef NO_LOOP
  }
#endif
  return result;
}
int main (void)
{
  for (unsigned i = 0; i != 32; ++i)
    {
      unsigned x = opaque (i * 42949671U);
#ifdef MUTABLE_PROGRAM
      a[0] = (x & 1) + 1;
      b[0] = (x & 7) + 1;
#endif
      if (evaluate (2,x) != expected (2,x)) __builtin_abort ();
    }
  return 0;
}
