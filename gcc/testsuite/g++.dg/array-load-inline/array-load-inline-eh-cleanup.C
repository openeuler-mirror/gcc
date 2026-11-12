/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4], observed;
struct watch
{
  __attribute__((noinline)) ~watch () { observed = tbl[2]; }
};
static void __attribute__((noinline))
init (int fail)
{
  if (fail)
    throw 1;
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
}
static void __attribute__((noinline, noclone))
run (int fail)
{
  watch w;
  try { init (fail); }
  catch (...) { throw; }
}
int main ()
{
  try { run (1); }
  catch (int) {}
  if (observed != 0)
    __builtin_abort ();
  run (0);
  if (observed != 9)
    __builtin_abort ();
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
