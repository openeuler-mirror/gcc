/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization  -ftree-immutable-loop-specialization -fdump-ipa-cp-details -fdump-tree-cunroll-details" } */
static const unsigned char program[130] = { [0 ... 128] = 1 };
__attribute__((noinline)) static unsigned
interpret (const unsigned char *p, unsigned x)
{
  while (*p) x = x * 3 + *p++;
  return x;
}
int main (void)
{
  unsigned expected1 = 7, expected2 = 9;
  for (unsigned i = 0; i != 129; ++i)
    {
      expected1 = expected1 * 3 + 1;
      expected2 = expected2 * 3 + 1;
    }
  if (interpret (program, 7) != expected1
      || interpret (program, 9) != expected2)
    __builtin_abort ();
  return 0;
}
/* { dg-final { scan-ipa-dump-not "Sharing immutable program" "cp" } } */
/* { dg-final { scan-tree-dump-not "Bounded immutable state peeling" "cunroll" } } */
