/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fno-component-type-flow -fno-unsafe-dynamic-cast-cache -fdump-tree-gimple" } */
#define CACHE_OPTIONS "unsafe-dynamic-cast-cache"
#include "dyncast-cache-function-options.h"
/* { dg-final { scan-tree-dump "__dcast_cache" "gimple" } } */
