/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization --param early-inlining-insns=0 --param ipa-cp-eval-threshold=10000000 -fdump-ipa-cp-details -fipa-value-specialization-shared-loop" } */
/* { dg-final { scan-ipa-dump-not "Budgeted shared loop value clone" "cp" } } */
#define NO_LOOP
#include "shared-loop-value-1.c"
