/* { dg-do compile } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -fipa-value-specialization -O3 -fno-inline -fipa-value-specialization-cold -fdump-ipa-cp-details" } */
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
__attribute__((cold)) int fixed1(const unsigned char *d,unsigned n) { return interpret((const unsigned char *)"a|b",d,n) + 1; }
__attribute__((cold)) int fixed2(const unsigned char *d,unsigned n) { return interpret((const unsigned char *)"a|b",d,n) + 2; }
__attribute__((cold)) int fixed3(const unsigned char *d,unsigned n) { return interpret((const unsigned char *)"a|b",d,n) + 3; }
__attribute__((cold)) int fixed4(const unsigned char *d,unsigned n) { return interpret((const unsigned char *)"a|b",d,n) + 4; }
