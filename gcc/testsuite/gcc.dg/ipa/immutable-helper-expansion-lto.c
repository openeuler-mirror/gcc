/* Exercise the bounded helper policy without changing inlining legality. */
/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fchecking=2 -fipa-value-specialization   --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 --param ipa-cp-eval-threshold=0 -fdump-ipa-inline-details -fipa-value-specialization-helper-expansion -flto -flto-partition=one" } */
/* { dg-require-effective-target lto } */
#include "immutable-inline-context-1.c"
/* { dg-final { scan-wpa-ipa-dump "Inlining inherited immutable program helper" "inline" } } */
