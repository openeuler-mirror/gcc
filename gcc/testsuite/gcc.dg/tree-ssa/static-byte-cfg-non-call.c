/* { dg-do compile } */
/* { dg-options "-O2 -ftree-static-byte-cfg -fnon-call-exceptions -fchecking=2 -fdump-tree-optimized" } */

/* Same eligible shape as static-byte-cfg-guarded.c.  The non-call-exception
   gate must prevent insertion of the parameter == 6 guard.  */
__attribute__ ((noipa)) unsigned
non_call (__SIZE_TYPE__ i, unsigned x)
{
  const unsigned char *p = (const unsigned char *) "abcdef" + i;
  if (*p == 0)
    return x + 1;
  return x + *p;
}

/* { dg-final { scan-tree-dump-not "i_.* == 6" "optimized" } } */
