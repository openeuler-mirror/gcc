/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -flto -flto-partition=one -fipa-value-specialization  -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-ipa-cp-details -fdump-ipa-inline-details" } */
#include "immutable-call-specialization-1.c"
/* { dg-final { scan-ipa-dump "Sharing immutable program" "cp" } } */
/* { dg-final { scan-ipa-dump "Inlining bounded immutable program helper" "inline" } } */
