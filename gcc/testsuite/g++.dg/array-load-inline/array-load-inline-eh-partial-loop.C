/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];
static void __attribute__((noinline))
maybe_throw (unsigned i)
{
  if (i == 2)
    throw 1;
}
static unsigned __attribute__((noinline, noclone))
read_tbl ()
{
  try
    {
      for (unsigned i = 0; i < 4; ++i)
        {
          maybe_throw (i);
          tbl[i] = i + 7;
        }
    }
  catch (int) { return tbl[3]; }
  return tbl[3];
}
int main ()
{
  if (read_tbl () != 0)
    __builtin_abort ();
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
