/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* Multiple loads revisit an SCC with an externally reachable member.
   Its stable failure must not become an initialized entry fact.  */
static unsigned tbl[4];
unsigned public_entry (unsigned, unsigned);
static unsigned __attribute__((noinline))
read_tbl (unsigned i, unsigned depth)
{
  if (depth)
    return public_entry (i, depth - 1);
  return tbl[i] + tbl[(i + 1) & 3];
}
unsigned __attribute__((noinline))
public_entry (unsigned i, unsigned depth)
{
  return read_tbl (i, depth);
}
static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 7;
}
int main (void)
{
  if (public_entry (1, 3) != 0)
    __builtin_abort ();
  init ();
  if (public_entry (1, 3) != 17)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "component is not fully visible" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
