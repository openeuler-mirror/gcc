/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp-clone -fpeel-loops -fipa-value-specialization --param ipa-cp-eval-threshold=0 --param ipa-value-specialization-peel-growth-budget=4096 --param ipa-value-specialization-peel-growth-percent=0 -fdump-tree-cunroll-details -fdump-tree-optimized" } */

extern unsigned observe_byte (unsigned, unsigned);

__attribute__((noinline))
static unsigned
interpret_pair (const unsigned char *a, const unsigned char *b, unsigned x)
{
  for (const unsigned char *p = a; *p; ++p)
    for (const unsigned char *q = b; *q; ++q)
      {
        x += observe_byte (*p, *q);
        x ^= observe_byte (*q, *p);
        x += observe_byte (*p + *q, x);
      }
  return x;
}

unsigned
evaluate_pair (unsigned x)
{
  static const unsigned char a[64]
    = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz?";
  static const unsigned char b[64]
    = "?zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA9876543210";
  return interpret_pair (a, b, x) + interpret_pair (b, a, x + 1);
}

/* An outer loop must be charged for the already expanded inner body.  */
/* { dg-final { scan-tree-dump "IPA readonly peeling growth:" "cunroll" } } */
/* { dg-final { scan-tree-dump "IPA readonly peeling growth budget exhausted" "cunroll" } } */
/* Each of the two clones has 63 copies of the three inner calls.  The
   outer loop must remain instead of multiplying those copies again.  */
/* { dg-final { scan-tree-dump-times {observe_byte \(} 378 "optimized" } } */
