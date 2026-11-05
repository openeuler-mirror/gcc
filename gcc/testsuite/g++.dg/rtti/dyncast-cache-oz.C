/* Size modes retain the documented opt-in cache policy.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-Oz -fcomponent-type-flow -fdump-tree-gimple" } */
#include "dyncast-cache1.C"
/* { dg-final { scan-tree-dump "__dcast_cache" "gimple" } } */
