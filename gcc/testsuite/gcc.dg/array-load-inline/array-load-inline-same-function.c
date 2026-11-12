/* { dg-do compile } */

static unsigned tbl[2][3];

static unsigned __attribute__((noinline))
f (unsigned i, unsigned j)
{
  for (unsigned a = 0; a < 2; ++a)
    for (unsigned b = 0; b < 3; ++b)
      tbl[a][b] = a * 10 + b;

  return tbl[i][j];
}

int
main (void)
{
  return f (1, 2) != 12;
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of tbl" "array-load-inline" } } */
