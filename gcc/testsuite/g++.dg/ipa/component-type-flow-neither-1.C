/* Explicitly disabling both mechanisms retains the original paths.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fno-ipa-icf -fno-component-type-flow -fno-unsafe-dynamic-cast-cache -fdump-tree-gimple -fdump-ipa-devirt-details" } */
#include "component-type-flow-independent.h"
/* { dg-final { scan-tree-dump-not "__dcast_cache" "gimple" } } */
/* { dg-final { scan-ipa-dump-not "Selecting dominant target" "devirt" } } */
