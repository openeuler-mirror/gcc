/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* A dominating store in the current iteration does not initialize the
   elements that will be written by later iterations.  */
static unsigned tbl[4];

static unsigned __attribute__((noinline))
test (void)
{
  unsigned sum = 0;
  for (unsigned i = 0; i < 4; ++i)
    {
      tbl[i] = i + 5;
      sum += tbl[(i + 1) & 3];
    }
  return sum;
}

int
main (void)
{
  if (test () != 5)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
