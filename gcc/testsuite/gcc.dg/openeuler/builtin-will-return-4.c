/* { dg-do compile } */
/* { dg-options "-O2 -fcode-hoisting -fbuiltin-will-return -fdump-tree-pre-details -ffat-lto-objects -fno-non-call-exceptions" } */
/* Flag notes as in builtin-will-return-1.c.  */

/* The whitelist's default arm, witnessed on its own: a built-in that
   is BUILT_IN_NORMAL, not const or pure, and NOVOPS - so it carries no
   vdef and value numbering cannot blame a clobber - but is not on the
   whitelist.  __builtin_speculation_safe_value is exactly that, and it
   is also why the arm is load-bearing: treating a speculation barrier
   as will-return would hoist the load above the barrier.  With the arm
   returning false, no insertion; flipping it to true inserts -
   measured both ways.  */

int
f (int c, int v, int *q)
{
  int r;
  if (c)
    {
      v = __builtin_speculation_safe_value (v);
      r = *q + v;
    }
  else
    r = *q + 2;
  return r;
}

/* { dg-final { scan-tree-dump-not {Inserted pretmp} "pre" } } */
