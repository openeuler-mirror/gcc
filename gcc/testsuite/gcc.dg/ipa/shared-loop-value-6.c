/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization --param early-inlining-insns=0 --param ipa-cp-eval-threshold=10000000 -fdump-ipa-cp-details -fipa-value-specialization-shared-loop -flto -flto-partition=one" } */
/* { dg-require-effective-target lto } */
/* { dg-final { scan-wpa-ipa-dump "Budgeted shared loop value clone" "cp" } } */
#include "shared-loop-value-1.c"
