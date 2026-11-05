/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details -fno-ipa-cp -fno-ipa-cp-clone" } */
/* { dg-require-ifunc "" } */

static unsigned tbl[4], observed;
typedef int (*fn) (void);
static int impl (void) { return 0; }
static fn __attribute__((noinline))
resolve (void)
{
  observed = tbl[2];
  return impl;
}
int indirect (void) __attribute__((ifunc ("resolve")));
static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
}
int main (void)
{
  if (indirect () || observed != 0)
    __builtin_abort ();
  init ();
  resolve ();
  if (observed != 9)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
