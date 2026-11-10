/* { dg-do compile } */

static unsigned tbl[4];

unsigned __attribute__((noinline))
f (unsigned i)
{
  for (unsigned a = 0; a < 4; ++a)
    tbl[a] = a + 1;

  tbl[2] = 99;
  return tbl[i];
}

int
main (void)
{
  return f (2) != 99;
}

/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: multiple store regions are not supported" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
