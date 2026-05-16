/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];

static void __attribute__((noinline))
init_tbl (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 13;
}

static unsigned __attribute__((noinline))
read_tbl_recursive (unsigned n, unsigned i)
{
  if (n)
    return read_tbl_recursive (n - 1, i);
  return tbl[i];
}

int
main (void)
{
  init_tbl ();
  return read_tbl_recursive (2, 1) != 14;
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: load of tbl is unreachable before initialization" "array-load-inline" } } */
