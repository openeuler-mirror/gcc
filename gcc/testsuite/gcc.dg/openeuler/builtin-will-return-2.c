/* { dg-do compile } */
/* { dg-options "-O2 -fcode-hoisting -fdump-tree-pre-details -ffat-lto-objects -fno-non-call-exceptions" } */
/* -ffat-lto-objects: the assertion below is a dump scan, and the dg
   layer adds that flag for scan-assembler* only, so under an -flto
   board the dump lands in LTRANS and the test reports UNRESOLVED.
   -fno-non-call-exceptions: under -fnon-call-exceptions the trapping
   load may throw and PRE declines the insertion for that reason,
   which is not what this test is about.  */

/* The control for builtin-will-return-1.c: same body, option off.  The
   default must stay exactly upstream's conservatism - the prefetch is
   treated as possibly not returning and the loads stay split.  If this
   ever starts inserting, either the default flipped or the flag guard
   in will_return_builtin_p rotted; -1.c alone could not tell.  */

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

/* { dg-final { scan-tree-dump-not {Inserted pretmp_[0-9]+ = \*q} "pre" } } */
