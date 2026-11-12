/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fno-peel-loops --param ipa-cp-eval-threshold=0 -fdump-ipa-cp-details -fipa-value-specialization" } */
/* { dg-final { scan-ipa-dump-not "Creating a specialized node of interpret" "cp" } } */
#define OPTION_PRAGMA_OFF
#include "value-specialization-function-options.h"
/* { dg-final { scan-ipa-dump-not "Adjusting immutable value benefit for interpret" "cp" } } */
