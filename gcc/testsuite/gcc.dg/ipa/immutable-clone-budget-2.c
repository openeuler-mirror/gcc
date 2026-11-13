/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization -fipa-value-specialization-clone-budget --param ipa-cp-eval-threshold=10000000 --param ipa-value-specialization-clone-growth-budget=0 -fdump-ipa-cp-details" } */
#include "immutable-call-specialization-1.c"
/* { dg-final { scan-ipa-dump-not "Budgeted immutable clone" "cp" } } */
