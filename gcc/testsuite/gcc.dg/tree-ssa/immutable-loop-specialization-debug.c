/* { dg-do compile } */
/* { dg-options "-O3 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fcompare-debug" } */

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
#define STEP(N) x = x * 2654435761u ^ (N); sink[N] = x;
#define B1(N) STEP(N)
#define B2(N) B1(N) B1((N)+1)
#define B4(N) B2(N) B2((N)+2)
#define B8(N) B4(N) B4((N)+4)
#define B16(N) B8(N) B8((N)+8)
#define B32(N) B16(N) B16((N)+16)
#define B64(N) B32(N) B32((N)+32)
#define B128(N) B64(N) B64((N)+64)
#define B256(N) B128(N) B128((N)+128)
#define B512(N) B256(N) B256((N)+256)
#define B1024(N) B512(N) B512((N)+512)
#define B2048(N) B1024(N) B1024((N)+1024)
static const unsigned char a[] = "\001\003\004\002", b[] = "\006\007\011\012";
static const unsigned char tab[] = "\001\002\003\004\005\001\002\003\004";
unsigned sink[2208];
__attribute__((noinline)) unsigned driver (unsigned x)
{
  unsigned r = interpret (a, x) + interpret (a, x + 1) + interpret (b, x) + interpret (b, x + 1);
  for (const unsigned char *q = tab; *q; ) x = x * 3 + *q++;
  B2048(0) B128(2048) B32(2176)
  return r + x;
}
