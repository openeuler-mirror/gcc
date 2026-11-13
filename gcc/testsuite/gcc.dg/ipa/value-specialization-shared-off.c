/* Disable the repeated-value budget so it does not claim this candidate
   before the shared loop reserve.  The master option supplies the other
   child defaults; explicit exclusions must win in either order.  */
/* { dg-final { scan-ipa-dump-not "Budgeted loop value clone:" "cp" } } */
/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-shared-loop -fipa-value-specialization --param early-inlining-insns=0 --param ipa-cp-eval-threshold=10000000 -fdump-ipa-cp-details" } */
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
static unsigned first (unsigned x)
{
  unsigned result = 0;
#ifndef NO_LOOP
  for (unsigned i = 0; i != (opaque(x) & 7) + 8; ++i)
#else
  { unsigned i = 0;
#endif
    result += interpret(a,x+i) + interpret(b,x+i);
#ifdef NO_LOOP
  }
#endif
  return result;
}
__attribute__((noinline,noclone))
static unsigned second (unsigned x)
{
  unsigned result = 0;
#ifndef NO_LOOP
  for (unsigned i = 0; i != (opaque(x) & 7) + 8; ++i)
#else
  { unsigned i = 0;
#endif
    result += interpret(a,x+3*i) + interpret(b,x+5*i);
#ifdef NO_LOOP
  }
#endif
  return result;
}
int main (void)
{
  for (unsigned j=0; j!=32; ++j)
    {
      unsigned x=opaque(j*42949671U), expected=0;
#ifdef MUTABLE_PROGRAM
      a[0]=(x&1)+1; b[0]=(x&7)+1;
#endif
#ifndef NO_LOOP
      for (unsigned i=0; i!=(x&7)+8; ++i)
#else
      { unsigned i=0;
#endif
	expected += reference(a,x+i)+reference(b,x+i)
		    +reference(a,x+3*i)+reference(b,x+5*i);
#ifdef NO_LOOP
      }
#endif
      if (first(x)+second(x)!=expected) __builtin_abort();
    }
  return 0;
}
