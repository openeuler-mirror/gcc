/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4], observed;
static void __attribute__((noinline, constructor))
observe (void)
{
  observed = tbl[2];
}
static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
}
int main (void)
{
  if (observed != 0)
    __builtin_abort ();
  init ();
  observe ();
  if (observed != 9)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */

/* { dg-final { scan-ipa-dump "load function is externally reachable" "array-load-inline" } } */
