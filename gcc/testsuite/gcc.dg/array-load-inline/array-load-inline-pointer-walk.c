/* { dg-do run } */

/* The pointer induction PHI contains the only table reference in sum.  */
static int tbl[16];

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
sum (int n)
{
  const int *p = tbl;
  int s = 0;
  while (n--)
    s += *p++;
  return s;
}

int
main (void)
{
  init ();
  if (get (5) != 16 || sum (16) != 376)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: address escapes" "array-load-inline" } } */
