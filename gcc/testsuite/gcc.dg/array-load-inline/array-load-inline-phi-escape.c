/* { dg-do run } */

/* CCP propagates the table address into a PHI argument.  */
static int tbl[16];
static int other[16];
int *leak;

static void __attribute__((noinline))
init (void)
{
  for (int i = 0; i < 16; ++i)
    tbl[i] = i * 3 + 1;
}

static int __attribute__((noinline))
get (int i)
{
  return tbl[i];
}

int __attribute__((noinline))
run (int c, int i)
{
  init ();
  int *p;
  if (c)
    p = tbl;
  else
    p = other;
  leak = p;
  return get (i);
}

int __attribute__((noinline))
read_leak (int i)
{
  return leak[i];
}

int
main (void)
{
  if (run (1, 5) != 16 || read_leak (5) != 16)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: address escapes" "array-load-inline" } } */
