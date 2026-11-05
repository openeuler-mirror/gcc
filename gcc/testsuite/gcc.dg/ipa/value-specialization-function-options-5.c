/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fno-peel-loops --param ipa-cp-eval-threshold=0 -fdump-ipa-cp-details -fipa-value-specialization" } */
#define SMALL_VALUE_SET
#define INTERPRET_ATTRIBUTES __attribute__((noinline, optimize ("no-ipa-value-specialization")))
#include "value-specialization-function-options.h"
/* Reach candidate evaluation without overflowing the ordinary lattice.  */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of interpret" 2 "cp" } } */
/* { dg-final { scan-ipa-dump-not "Adjusting immutable value benefit for interpret" "cp" } } */
