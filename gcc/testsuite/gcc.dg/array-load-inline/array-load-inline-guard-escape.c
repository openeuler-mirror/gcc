/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */
/* { dg-additional-sources "inputs/guard-escape.c" } */

/* The indirect guard write must not be omitted from the proof.  */
static unsigned tbl[4], guard[2];
extern void opaque (unsigned *);

static void __attribute__((noinline, used))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 5;
}

void __attribute__((noinline))
publish (unsigned i)
{
  opaque (&guard[i]);
}

static unsigned __attribute__((noinline))
get (unsigned i)
{
  if (guard[0])
    return tbl[i];
  return 0;
}

int
main (void)
{
  publish (0);
  if (get (2) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
