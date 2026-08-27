/* { dg-do compile} */
/* { dg-options "-O2 -fdump-tree-evrp1-details -fno-tree-fre -fno-tree-ccp -fno-tree-forwprop" } */

int f(int x)
{
    x = x|1;
    return x & 1;
}

/* { dg-final { scan-tree-dump "Folded into: return 1;" "evrp1" } }  */
