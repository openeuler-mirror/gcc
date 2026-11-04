/* { dg-do run } */
/* { dg-options "-fno-ipa-immutable-clone-budget -fno-ipa-loop-value-specialization -fno-ipa-shared-loop-value-specialization -fno-ipa-bounded-cold-immutable-specialization -fno-ipa-immutable-helper-expansion -O3 -fipa-value-specialization  -ftree-immutable-loop-specialization" } */
static volatile unsigned visits;
static unsigned step (const unsigned char *&p, unsigned x)
{
  ++visits;
  unsigned op = *p++;
  if (op == 255) throw x;
  return x + op;
}
__attribute__((noinline)) static unsigned
interpret (const unsigned char *p, unsigned x)
{
  while (*p) x = step (p, x);
  return x;
}
static const unsigned char a[] = {1, 3, 255, 0};
static const unsigned char b[] = {2, 5, 7, 0};
int main ()
{
  for (unsigned i = 0; i < 100; ++i)
    {
      visits = 0;
      try
	{
	  interpret (a, i);
	  __builtin_abort ();
	}
      catch (unsigned n)
	{
	  if (n != i + 4 || visits != 3) __builtin_abort ();
	}
      visits = 0;
      try
	{
	  interpret (a, i + 1);
	  __builtin_abort ();
	}
      catch (unsigned n)
	{
	  if (n != i + 5 || visits != 3) __builtin_abort ();
	}
      visits = 0;
      if (interpret (b, i) != i + 14
	  || interpret (b, i + 1) != i + 15 || visits != 6)
	__builtin_abort ();
    }
}
