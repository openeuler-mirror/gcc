/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */
/* { dg-additional-sources "inputs/external-caller.c" } */

/* public_get also has callers in another translation unit.  */
static unsigned tbl[4];

static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 5;
}

static unsigned __attribute__((noinline))
get (unsigned i)
{
  return tbl[i];
}

unsigned __attribute__((noinline))
public_get (unsigned i)
{
  return get (i);
}

unsigned __attribute__((noinline))
public_run (unsigned i)
{
  init ();
  return public_get (i);
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
