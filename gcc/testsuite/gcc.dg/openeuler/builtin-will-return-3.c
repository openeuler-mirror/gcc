/* { dg-do compile } */
/* { dg-options "-O2 -fcode-hoisting -fbuiltin-will-return -fdump-tree-pre-details -ffat-lto-objects" } */
/* -ffat-lto-objects: the assertion below is a dump scan, and the dg
   layer adds that flag for scan-assembler* only, so under an -flto
   board the dump lands in LTRANS and the test reports UNRESOLVED.  */

/* The predicate must not leak past built-ins: an ordinary external
   call fails will_return_builtin_p's gimple_call_builtin_p guard
   before the whitelist switch is even consulted, so even with
   -fbuiltin-will-return in force PRE keeps the loads split.  The load
   is from a const table so that value numbering sees through the
   call's clobber - measured, with the predicate mutated to constant
   true this very shape does insert - which means what stands between
   the two loads here is the may-not-return assumption alone, and this
   test discriminates.  The whitelist's own default arm is pinned
   separately by builtin-will-return-4.c.  */

void ext (void);

static const int table[4] = { 1, 2, 3, 4 };

int
h (int c, int i)
{
  int r;
  if (c)
    {
      ext ();
      r = table[i] + 1;
    }
  else
    r = table[i] + 2;
  return r;
}

/* { dg-final { scan-tree-dump-not {Inserted pretmp} "pre" } } */
