/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fno-peel-loops --param ipa-cp-eval-threshold=0 --param ipa-cp-value-list-size=2 -fdump-ipa-cp-details -fipa-value-specialization" } */
/* Keep the useful immutable entries when later ordinary pointer values arrive.
   The existing raised limit is shared by this complete lattice.  */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of interpret" 17 "cp" } } */
#include "value-specialization-function-options.h"
