/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* The nonzero edge reaches the join, but does not dominate it.  */
static unsigned tbl[4], guard[1];
static volatile unsigned sink;

static void __attribute__((noinline, used))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 5;
  guard[0] = 1;
}

static unsigned __attribute__((noinline))
get (unsigned i)
{
  if (!guard[0])
    sink++;
  return tbl[i];
}

int
main (void)
{
  if (get (2) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
