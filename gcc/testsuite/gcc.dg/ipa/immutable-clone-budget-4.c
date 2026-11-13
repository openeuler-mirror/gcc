/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization  -ftree-immutable-loop-specialization -fipa-value-specialization-clone-budget --param ipa-cp-eval-threshold=10000000 -fdump-ipa-cp-details" } */
/* A known address does not make a mutable program an immutable object.  */
#include "immutable-call-specialization-2.c"
/* { dg-final { scan-ipa-dump-not "Budgeted immutable clone:" "cp" } } */
/* { dg-final { scan-ipa-dump-not "Sharing immutable program" "cp" } } */
