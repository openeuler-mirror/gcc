/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];
static unsigned guard[1];

static void __attribute__((noinline, used))
init_tbl (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 17;
}

static void __attribute__((noinline))
publish_guard_without_init (void)
{
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
  publish_guard_without_init ();
  return maybe_read_tbl (2);
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: init does not dominate load" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: load of tbl is unreachable before initialization" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
