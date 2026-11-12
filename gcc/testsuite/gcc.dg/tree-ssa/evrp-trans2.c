/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp1" } */

#define int unsigned
#include "evrp-trans.c"

/* { dg-final { scan-tree-dump-not "kill" "evrp1" } }  */
/* { dg-final { scan-tree-dump-times "keep" 13 "evrp1"} } */
