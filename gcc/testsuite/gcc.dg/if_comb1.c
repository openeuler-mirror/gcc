/* { dg-do compile } */
/* { dg-options "-Ofast -S --param=merge-assign-stmts-ifcombine=1 -fdump-tree-ifcombine" } */

int foo (double a, double b, int c)
{
    if (c < 10 || a - b > 1.0)
        return 0;
    else 
        return 1;
}

/* { dg-final { scan-tree-dump "optimizing two comparisons" "ifcombine"} } */
/* { dg-final { scan-tree-dump "Merging blocks" "ifcombine"} } */
