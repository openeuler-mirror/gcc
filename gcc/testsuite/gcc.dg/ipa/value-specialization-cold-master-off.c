/* { dg-do compile } */
/* { dg-options "-O3 -fno-inline -fipa-value-specialization-cold -fno-ipa-value-specialization -fdump-ipa-cp-details" } */
/* A cold wrapper is an admission test, not a runtime profile claim.  */
__attribute__((noinline)) int
interpret (const unsigned char *p, const unsigned char *data, unsigned n)
{
  if (!p) return -1;
  unsigned i = 0;
  int r = 0;
  while (i < n && p[i])
    {
      if (p[i] == data[i]) r += 3;
      else if (p[i] == '|') r -= 2;
      else r += 1;
      ++i;
    }
  return r;
}
int dynamic (const unsigned char *p, const unsigned char *d, unsigned n)
{ return interpret (p, d, n); }
__attribute__((cold)) int fixed (const unsigned char *d, unsigned n)
{ return interpret ((const unsigned char *) "a|b", d, n); }
/* { dg-final { scan-ipa-dump-not "Bounded cold immutable clone" "cp" } } */
