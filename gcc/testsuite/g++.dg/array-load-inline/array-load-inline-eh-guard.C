/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4], ready[1];
static void __attribute__((noinline))
init (int fail)
{
  if (fail)
    throw 1;
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
}
static unsigned __attribute__((noinline, noclone))
read_tbl (unsigned i)
{
  return ready[0] ? tbl[i] : 0;
}
static unsigned __attribute__((noinline, noclone))
publish (int fail)
{
  ready[0] = 1;
  try { init (fail); }
  catch (int) { return read_tbl (2); }
  return read_tbl (2);
}
int main ()
{
  if (publish (1) != 0 || publish (0) != 9)
    __builtin_abort ();
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
