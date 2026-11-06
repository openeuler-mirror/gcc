/* { dg-do compile } */
/* { dg-options "-O2 -ftree-static-byte-cfg -fchecking=2 -fdump-tree-static-byte-cfg-details" } */

extern volatile unsigned observable;

__attribute__((noipa)) unsigned
with_effects (unsigned key)
{
  const unsigned char *p = (const unsigned char *) "bounded immutable bytes";
  unsigned sum = 0;
  while (*p)
    {
      sum += *p++ ^ key;
      sum += observable;
    }
  return sum;
}

/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT" "static-byte-cfg" } } */

/* { dg-final { scan-tree-dump "GUARDPLAN with_effects .*reason=effects-eh-or-unsupported" "static-byte-cfg" } } */
