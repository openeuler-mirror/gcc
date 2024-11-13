/* { dg-do compile } */
/* { dg-options "-Ofast -S --param=merge-assign-stmts-ifcombine=1 -fdump-tree-ifcombine" } */

int foo (int a, int b, int c)
{
    if (a > 1 && b + c < 10)
        a++;
    return a;
}

/* { dg-final { scan-tree-dump "optimizing two comparisons" "ifcombine"} } */
/* { dg-final { scan-tree-dump "Merging blocks" "ifcombine"} } */
