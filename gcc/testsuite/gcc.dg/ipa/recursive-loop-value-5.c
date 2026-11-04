/* { dg-do compile } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization -fipa-value-specialization-loop --param early-inlining-insns=0 --param ipa-cp-eval-threshold=10000000 -fdump-ipa-cp-details" } */
/* { dg-final { scan-ipa-dump-not "Recursive caller loop context for immutable value" "cp" } } */
/* { dg-final { scan-ipa-dump-not "Budgeted loop value clone:" "cp" } } */
__attribute__((noinline))
static unsigned leaf (const unsigned char *p, unsigned x)
{
  if (!p || !*p) return x;
  if (x > 1) return leaf (p,x>>1) ^ leaf (p,x>>2);
  return *p + x;
}
__attribute__((noinline,noclone))
unsigned controller (unsigned depth, unsigned x)
{
  unsigned result = 0;
  if (depth) result = controller (depth-1,x+1) ^ controller (depth-1,x+7);
  for (unsigned i = 0; i != x; ++i)
    result += leaf ((const unsigned char *) "abcxyz",i);
  return result;
}
