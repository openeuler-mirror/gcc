/* { dg-do run } */
/* { dg-options "-O3 -fPIC -fsemantic-interposition -fdump-ipa-cp-details -fdump-tree-optimized -fipa-cp-clone -fipa-value-specialization -flto -flto-partition=one" } */
/* { dg-require-effective-target fpic } */
/* { dg-require-weak-override "" } */
/* { dg-require-effective-target lto } */
/* { dg-additional-sources "value-specialization-interposable-aux.c" } */
extern unsigned read_interposed (unsigned);
const unsigned char interposed_program[] = {7, 9, 11, 0};
static const volatile unsigned char volatile_program[] = "\004\006";
static const volatile unsigned char volatile_program2[] = "\005\007";
__attribute__((noinline)) static unsigned read_volatile (const volatile unsigned char *p, unsigned x)
{
  unsigned result = x;
  while (*p) { result = result * 3 + *p; ++p; }
  return result;
}
int main (void)
{
  for (unsigned x = 0; x != 16; ++x)
    if (read_interposed (x) != ((x * 3 + 7) * 3 + 9) * 3 + 11
        || read_volatile (volatile_program, x) != (x * 3 + 4) * 3 + 6
        || read_volatile (volatile_program2, x) != (x * 3 + 5) * 3 + 7)
      __builtin_abort ();
  return 0;
}
/* Keep the volatile memory accesses even when constant pointer arguments
   expose object identities to IPA.  A fixed-value oracle alone is insufficient.  */
/* { dg-final { scan-ltrans-tree-dump " ={v} " "optimized" } } */
/* Prove that the constant pointer arguments reach the specialization path.  */
/* { dg-final { scan-wpa-ipa-dump "Creating a specialized node of read_volatile" "cp" } } */
/* { dg-final { scan-wpa-ipa-dump "Adjusting immutable value benefit for read_volatile" "cp" } } */
