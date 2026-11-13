/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fcomponent-type-flow -funsafe-dynamic-cast-cache -fdump-tree-gimple" } */
#define CACHE_OPTIONS "no-unsafe-dynamic-cast-cache"
#include "dyncast-cache-function-options.h"
/* { dg-final { scan-tree-dump-not "__dcast_cache" "gimple" } } */
