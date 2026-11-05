/* Explicitly enabling the feature at -O0 does not create RTTI caches.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O0 -fcomponent-type-flow -fdump-tree-gimple" } */
#include "dyncast-cache1.C"
/* { dg-final { scan-tree-dump-not "__dcast_cache" "gimple" } } */
