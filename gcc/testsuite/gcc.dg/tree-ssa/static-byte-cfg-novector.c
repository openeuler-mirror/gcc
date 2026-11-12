/* { dg-do compile } */
/* { dg-options "-O3 -ftree-static-byte-cfg -fdump-tree-vect-details -fdump-tree-static-byte-cfg-details" } */
__attribute__ ((noipa)) unsigned
nv_guarded (__SIZE_TYPE__ i, const unsigned *a, unsigned n)
{
  const unsigned char *p = (const unsigned char *) "abcdef" + i;
  unsigned s = 0;
#pragma GCC novector
  for (unsigned j = 0; j < n; j++)
    s += a[j];
  if (*p == 0)
    return s + 1;
  return s + *p;
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

/* { dg-final { scan-tree-dump "RESIDUAL-REJECT nv_guarded annotated-loop" "static-byte-cfg" } } */
