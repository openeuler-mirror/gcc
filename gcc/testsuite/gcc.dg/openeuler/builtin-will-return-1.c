/* { dg-do compile } */
/* { dg-options "-O2 -fcode-hoisting -fbuiltin-will-return -fdump-tree-pre-details -ffat-lto-objects -fno-non-call-exceptions" } */
/* -ffat-lto-objects: the assertion below is a dump scan, and the dg
   layer adds that flag for scan-assembler* only, so under an -flto
   board the dump lands in LTRANS and the test reports UNRESOLVED.
   -fno-non-call-exceptions: under -fnon-call-exceptions the trapping
   load may throw and PRE declines the insertion for that reason,
   which is not what this test is about.  */

/* Both arms load *q; the then-arm loads it after a prefetch.  A prefetch
   is not const or pure, so by default PRE must assume it might not
   return and may not anticipate the trapping load across it - the
   redundancy survives (builtin-will-return-2.c pins that).  With
   -fbuiltin-will-return the whitelist says the call always returns and
   PRE unifies the loads; the insertion line below is the witness.  */

int
f (int c, int *p, int *q)
{
  int r;
  if (c)
    {
      __builtin_prefetch (p);
      r = *q + 1;
    }
  else
    r = *q + 2;
  return r;
}

/* { dg-final { scan-tree-dump {Inserted pretmp_[0-9]+ = \*q} "pre" } } */
