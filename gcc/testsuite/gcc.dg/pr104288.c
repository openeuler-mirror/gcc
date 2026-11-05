/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp1 -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" { keeps_null_pointer_checks } } */

void keep(int result) __attribute__((noipa));
void keep(int result)
{
    if (result)
        __builtin_exit(0);
}

void foo (void *p) __attribute__((nonnull(1)));

void bar (void *p)
{
  keep (p == 0);
  foo (p);
  if (!p)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "abort" "evrp1" } } */
/* { dg-final { scan-tree-dump-times  "== 0B;" 1 "evrp1" } } */
