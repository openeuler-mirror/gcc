/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */
/* { dg-require-effective-target int32plus } */

/* Check every intermediate conversion, not just the final wide type.  */
static unsigned tbl[512];

static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 512; ++i)
    tbl[(unsigned long long) (__UINT8_TYPE__) i] = 11;
}

static unsigned __attribute__((noinline))
get (unsigned i)
{
  return tbl[i];
}

int
main (void)
{
  init ();
  if (get (255) != 11 || get (300) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: removed dead store to tbl" "array-load-inline" } } */
