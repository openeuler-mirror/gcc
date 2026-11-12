/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* Do not truncate this dimension to four elements.  Compile only, to avoid
   requiring a large virtual address space on the execution target.  */
static unsigned char tbl[4294967300ULL];

static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = 5;
}

static unsigned __attribute__((noinline))
get (unsigned long i)
{
  return tbl[i];
}

int
main (void)
{
  init ();
  return get (4294967298ULL) != 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
