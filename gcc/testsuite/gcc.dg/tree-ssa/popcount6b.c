// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp1 -fno-tree-ccp" }

#include "popcount6.c"

// { dg-final { scan-tree-dump "return 1;" "evrp1" } }
