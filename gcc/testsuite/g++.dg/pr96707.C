/* { dg-do compile} */
/* { dg-options "-O2 -fdump-tree-evrp1" } */

bool f(unsigned x, unsigned y)
{
    return (x >> y) <= x;
}

/* { dg-final { scan-tree-dump "return 1" "evrp1" } }  */

