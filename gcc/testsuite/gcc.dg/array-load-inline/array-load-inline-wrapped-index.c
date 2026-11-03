/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */
/* { dg-require-effective-target int32plus } */

/* A widening conversion cannot undo wrapping of the underlying IV.  */
static unsigned tbl[512];

static void __attribute__((noinline))
init (void)
{
  __UINT8_TYPE__ index = 0;
  for (unsigned i = 0; i < 512; ++i, ++index)
    tbl[(unsigned long long) index] = 7;
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
  if (get (255) != 7 || get (300) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: removed dead store to tbl" "array-load-inline" } } */
