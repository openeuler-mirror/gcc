/* The expanded helper allowance must retain the same stack check.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fchecking=2 -fipa-value-specialization -fno-ipa-value-specialization-helper-expansion --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 --param ipa-cp-eval-threshold=0 -fdump-ipa-inline-details -fipa-value-specialization-helper-expansion -fstack-usage" } */
#include "immutable-helper-stack.c"
/* { dg-final { scan-ipa-dump "large-stack-frame-growth limit reached" "inline" } } */
/* { dg-final { scan-ipa-dump-not "Inlining bounded immutable program helper" "inline" } } */
