/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4] __attribute__((used));

static unsigned __attribute__((noinline))
f (unsigned i)
{
  for (unsigned a = 0; a < 4; ++a)
    tbl[a] = a + 53;

  return tbl[i];
}

int
main (void)
{
  return f (1) != 54;
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: array references are not fully explicit" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: removed dead store to tbl" "array-load-inline" } } */
