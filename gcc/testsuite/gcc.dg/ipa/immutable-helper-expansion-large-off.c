/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization  --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 --param ipa-cp-eval-threshold=0 -fdump-ipa-cp-details -fdump-ipa-inline-details  -fchecking=2 -fno-ipa-value-specialization-helper-expansion" } */
#include "immutable-helper-expansion-large.c"
/* { dg-final { scan-ipa-dump-not "Inlining inherited immutable program helper" "inline" } } */
