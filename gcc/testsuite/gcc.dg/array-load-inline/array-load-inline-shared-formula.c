/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* Repeated squaring forms a small DAG, but an exponentially large tree.  */
static unsigned tbl[4];

static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    {
      unsigned x = i + 1;
#define STEP x = x * x + i;
      STEP STEP STEP STEP STEP
      STEP STEP STEP STEP STEP
      STEP STEP STEP STEP STEP
      STEP STEP STEP STEP STEP
#undef STEP
      tbl[i] = x;
    }
}

static unsigned __attribute__((noinline))
get (unsigned i)
{
  return tbl[i];
}

int
main (void)
{
  init ();
  for (unsigned i = 0; i < 4; ++i)
    {
      unsigned expected = i + 1;
      for (unsigned j = 0; j < 20; ++j)
        expected = expected * expected + i;
      if (get (i) != expected)
        __builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of tbl" "array-load-inline" } } */
