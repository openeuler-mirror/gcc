/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fipa-value-specialization -fno-ipa-value-specialization -fno-tree-immutable-loop-specialization -fdump-ipa-cp-details -fdump-ipa-inline-details -fdump-tree-cunroll-details" } */
#include "immutable-call-specialization-1.c"
/* { dg-final { scan-ipa-dump-not "Sharing immutable program" "cp" } } */
/* { dg-final { scan-ipa-dump-not "Inlining bounded immutable program helper" "inline" } } */
/* { dg-final { scan-tree-dump-not "Bounded immutable state peeling" "cunroll" } } */
