/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization -fipa-value-specialization-loop --param early-inlining-insns=0 --param ipa-cp-eval-threshold=10000000 -fdump-ipa-cp-details --param ipa-cp-large-unit-insns=1 --param ipa-cp-unit-growth=0 --param ipa-value-specialization-loop-growth-budget=0" } */
#include "immutable-loop-value-1.c"
/* { dg-final { scan-ipa-dump-not "Budgeted loop value clone:" "cp" } } */
