/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4], expected;
static void __attribute__((noinline, destructor))
observe (void)
{
  if (tbl[2] != expected)
    __builtin_abort ();
}
static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
}
int main (int argc, char **argv)
{
  if (argc == 1)
    return 0;
  init ();
  expected = 9;
  observe ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
