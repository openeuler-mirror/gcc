/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp-clone -fpeel-loops -fipa-value-specialization --param ipa-cp-eval-threshold=0 --param ipa-value-specialization-peel-growth-budget=256 --param ipa-value-specialization-peel-growth-percent=0 -fdump-tree-cunroll-details -fdump-tree-optimized" } */

extern unsigned observe_byte (unsigned, unsigned);

__attribute__((noinline))
static unsigned
interpret_two (const unsigned char *a, const unsigned char *b, unsigned x)
{
  for (const unsigned char *p = a; *p; ++p)
    {
      x += observe_byte (*p, x);
      x ^= observe_byte (*p, x);
      x += observe_byte (*p, x);
    }
  for (const unsigned char *q = b; *q; ++q)
    {
      x += observe_byte (*q, x);
      x ^= observe_byte (*q, x);
      x += observe_byte (*q, x);
    }
  return x;
}

unsigned
evaluate_two (unsigned x)
{
  static const unsigned char a[8] = "abcdefg";
  static const unsigned char b[8] = "7654321";
  return interpret_two (a, b, x) + interpret_two (b, a, x + 1);
}

/* A successful first peel must leave its charge for the sibling loop.  */
/* { dg-final { scan-tree-dump "IPA readonly peeling growth:" "cunroll" } } */
/* { dg-final { scan-tree-dump {IPA readonly peeling growth budget exhausted: [1-9][0-9]* of 256 units already used} "cunroll" } } */
/* Each clone has seven copies of three calls in the first peeled loop,
   and three calls in the sibling that retains its residual loop.  */
/* { dg-final { scan-tree-dump-times {observe_byte \(} 48 "optimized" } } */
