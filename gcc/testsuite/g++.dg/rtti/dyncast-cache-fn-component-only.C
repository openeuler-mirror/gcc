/* A function-specific ordinary option must not enable the unsafe cache.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fdump-tree-gimple" } */
#define CACHE_OPTIONS "component-type-flow"
#include "dyncast-cache-function-options.h"
/* { dg-final { scan-tree-dump-not "__dcast_cache" "gimple" } } */
