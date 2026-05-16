/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];
static unsigned guard[1];

static void __attribute__((noinline, used))
init_tbl (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 3;
}

static void __attribute__((noinline, used))
publish_guard (void)
{
  init_tbl ();
  guard[0] = 1;
}

static unsigned __attribute__((noinline))
maybe_read_tbl (unsigned i)
{
  if (guard[0])
    return tbl[i];
  return 0;
}

int
main (void)
{
  return maybe_read_tbl (2);
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: load of tbl is unreachable before initialization" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: tbl loads are either strictly dominated by initialization or guarded before initialization" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of tbl" "array-load-inline" } } */
