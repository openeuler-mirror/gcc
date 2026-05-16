/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];

static void __attribute__((noinline))
init_tbl (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
}

static unsigned __attribute__((noinline))
sum_tbl (unsigned i, unsigned j)
{
  return tbl[i] + tbl[j];
}

int
main (void)
{
  init_tbl ();
  return sum_tbl (1, 2) != 17;
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-times "array-load-inline: inlined load of tbl" 2 "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: load of tbl is unreachable before initialization" "array-load-inline" } } */
