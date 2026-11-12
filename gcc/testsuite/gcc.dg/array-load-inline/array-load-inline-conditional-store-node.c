/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];

static void __attribute__((noinline))
init_tbl (int flag)
{
  if (flag)
    for (unsigned i = 0; i < 4; ++i)
      tbl[i] = i + 43;
}

static unsigned __attribute__((noinline))
read_tbl (unsigned i)
{
  return tbl[i];
}

int
main (int argc, char **argv)
{
  (void) argv;
  init_tbl (argc);
  return read_tbl (2);
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: init does not dominate load" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
