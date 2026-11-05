/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fno-peel-loops --param ipa-cp-eval-threshold=0 -fdump-ipa-cp-details -fno-ipa-value-specialization" } */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of interpret" 17 "cp" } } */
#define OPTION_PRAGMA_ON
#include "value-specialization-function-options.h"
/* { dg-final { scan-ipa-dump "Adjusting immutable value benefit for interpret" "cp" } } */
