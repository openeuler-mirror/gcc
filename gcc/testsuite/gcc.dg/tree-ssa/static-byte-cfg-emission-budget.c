/* { dg-do compile } */
/* { dg-options "-O2 -ftree-static-byte-cfg -fchecking=2 -fdump-tree-static-byte-cfg-details" } */

extern unsigned step (unsigned) __attribute__ ((const, nothrow));
#define STEP sum = step (sum);
#define S4 STEP STEP STEP STEP
#define S16 S4 S4 S4 S4
#define S64 S16 S16 S16 S16
#define S256 S64 S64 S64 S64

__attribute__ ((noipa)) unsigned
emission_budget (unsigned key)
{
  const unsigned char *p = (const unsigned char *) "abcdefgh";
  unsigned sum = key;
  while (*p)
    {
      S256
      sum += *p++;
    }
  return sum;
}

/* { dg-final { scan-tree-dump "GUARDPLAN emission_budget .*status=ready" "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump "RESIDUAL-REJECT emission_budget emission-budget" "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT" "static-byte-cfg" } } */
