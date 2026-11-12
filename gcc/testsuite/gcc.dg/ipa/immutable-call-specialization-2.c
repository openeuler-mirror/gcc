/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization  -fdump-ipa-cp-details -fdump-ipa-inline-details" } */
static unsigned char program[5] = {1, 2, 3, 4, 0};
__attribute__((noinline)) static unsigned
interpret (const unsigned char *p, unsigned x)
{
  while (*p)
    x = x * 3 + *p++;
  return x;
}
int main (void)
{
  for (unsigned n = 0; n != 50; ++n)
    {
      program[2] = n + 1;
      unsigned expected = ((((7 * 3 + 1) * 3 + 2) * 3 + n + 1) * 3 + 4);
      if (interpret (program, 7) != expected)
	__builtin_abort ();
    }
  return 0;
}
/* Mutable storage must not be promoted into an immutable program.  */
/* { dg-final { scan-ipa-dump-not "Sharing immutable program" "cp" } } */
/* { dg-final { scan-ipa-dump-not "Inlining bounded immutable program helper" "inline" } } */
