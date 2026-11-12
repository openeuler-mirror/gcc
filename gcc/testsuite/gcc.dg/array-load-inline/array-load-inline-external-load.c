/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];

static void __attribute__((noinline))
init_tbl (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 47;
}

unsigned __attribute__((noinline))
read_tbl (unsigned i)
{
  return tbl[i];
}

int
main (void)
{
  init_tbl ();
  return read_tbl (2) != 49;
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: load function is externally reachable" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
