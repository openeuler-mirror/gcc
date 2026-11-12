/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];
static void __attribute__((noinline))
init (int fail)
{
  if (fail)
    throw 1;
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
}
static unsigned __attribute__((noinline, noclone))
read_tbl (int fail)
{
  try { init (fail); }
  catch (int) { return tbl[2]; }
  return tbl[2];
}
int main ()
{
  if (read_tbl (1) != 0 || read_tbl (0) != 9)
    __builtin_abort ();
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
