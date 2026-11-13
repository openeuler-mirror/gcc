/* Opting into the cache must not select an additional virtual target.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fno-ipa-icf -fno-component-type-flow -funsafe-dynamic-cast-cache -fdump-tree-gimple -fdump-ipa-devirt-details" } */
#include "component-type-flow-independent.h"
/* { dg-final { scan-tree-dump "__dcast_cache" "gimple" } } */
/* { dg-final { scan-ipa-dump-not "Selecting dominant target" "devirt" } } */
