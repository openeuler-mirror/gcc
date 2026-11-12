// PR tree-optimization/83541
// { dg-do compile }
// { dg-options "-O3 -std=c++17 -ffast-math -fdump-tree-evrp1"  }

#include <limits>

int test(int x)
{
    if(x == std::numeric_limits<int>::max())
    {
        return x+1;
    }
    return 42;
}

// { dg-final { scan-tree-dump "return 42"  evrp1 } }
// { dg-final { scan-tree-dump-not "return _"  evrp1 } }
