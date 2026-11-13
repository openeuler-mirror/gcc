/* The ordinary option must not introduce a lifetime-dependent RTTI cache.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fno-ipa-icf -fcomponent-type-flow -fdump-tree-gimple -fdump-ipa-devirt-details" } */
#include "component-type-flow-independent.h"
/* { dg-final { scan-tree-dump-not "__dcast_cache" "gimple" } } */
/* { dg-final { scan-ipa-dump "Selecting dominant target \\(4 versus 2 live vtables\\)" "devirt" } } */
