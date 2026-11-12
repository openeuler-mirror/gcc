/* { dg-do compile } */
/* { dg-options "-O2 -ftree-static-byte-cfg -fchecking=2 -fdump-tree-static-byte-cfg-details" } */

#define S16 "abcdefghijklmnop"
#define S64 S16 S16 S16 S16
#define S256 S64 S64 S64 S64
extern unsigned step (unsigned) __attribute__ ((const, nothrow));

__attribute__ ((noipa)) unsigned
state_budget (unsigned key)
{
  const unsigned char *p = (const unsigned char *) (S256 S256);
  unsigned sum = key;
  while (*p)
    sum += step (*p++ ^ key);
  return sum;
}

/* { dg-final { scan-tree-dump "GUARDPLAN state_budget .*reason=state-budget" "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT" "static-byte-cfg" } } */
