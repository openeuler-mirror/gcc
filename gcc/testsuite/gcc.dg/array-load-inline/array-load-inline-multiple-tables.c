/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* Reuse function analyses across candidates after rewriting the first table.  */
static unsigned first[4], second[4];

static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    {
      first[i] = i + 9;
      second[i] = i * 3 + 7;
    }
}

static unsigned __attribute__((noinline))
get (unsigned i)
{
  return first[i] + second[i];
}

int
main (void)
{
  init ();
  for (unsigned i = 0; i < 4; ++i)
    if (get (i) != i * 4 + 16)
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of first" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of second" "array-load-inline" } } */
