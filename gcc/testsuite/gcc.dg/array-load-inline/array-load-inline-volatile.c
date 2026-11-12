/* { dg-do compile } */

static volatile unsigned tbl[4];

unsigned __attribute__((noinline))
f (unsigned i)
{
  for (unsigned a = 0; a < 4; ++a)
    tbl[a] = a + 1;

  return tbl[i];
}

int
main (void)
{
  return f (1) != 2;
}

/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: volatile access" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
