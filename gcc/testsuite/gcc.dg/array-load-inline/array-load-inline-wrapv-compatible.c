/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details -fno-ipa-cp -fno-ipa-cp-clone" } */

/* Moving a wrapping signed multiply must preserve its overflow semantics.  */
static int tbl[4];
static void __attribute__((noinline, optimize ("wrapv")))
init (void)
{
  for (int i = 0; i < 4; ++i)
    tbl[i] = i * 1000000000;
}
static int __attribute__((noinline, optimize ("wrapv")))
read_tbl (int i)
{
  return i >= 0 && i < 4 ? tbl[i] < 0 : 0;
}
int main (void)
{
  init ();
  if (!read_tbl (3) || read_tbl (1))
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of tbl" "array-load-inline" } } */
