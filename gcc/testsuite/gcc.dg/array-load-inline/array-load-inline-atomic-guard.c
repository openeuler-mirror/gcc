/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details -fno-ipa-cp -fno-ipa-cp-clone" } */

static unsigned tbl[4], ready[1];
static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
  __atomic_store_n (&ready[0], 1, __ATOMIC_RELEASE);
}
static unsigned __attribute__((noinline))
read_tbl (unsigned i)
{
  return __atomic_load_n (&ready[0], __ATOMIC_ACQUIRE) ? tbl[i] : 0;
}
int main (void)
{
  if (read_tbl (2) != 0)
    __builtin_abort ();
  init ();
  if (read_tbl (2) != 9)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
