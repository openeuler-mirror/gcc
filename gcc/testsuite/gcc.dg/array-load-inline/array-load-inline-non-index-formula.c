/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];

static void __attribute__((noinline))
init_tbl (unsigned base)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = base + i;
}

static unsigned __attribute__((noinline))
read_tbl (unsigned i)
{
  return tbl[i];
}

int
main (void)
{
  init_tbl (23);
  return read_tbl (1) != 24;
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: no loads found for tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
