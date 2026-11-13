/* A single policy limit must independently reject helper expansion.  */
/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -fipa-value-specialization -fno-ipa-value-specialization-helper-expansion --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 --param ipa-cp-eval-threshold=0 -fdump-ipa-inline-details --param ipa-value-specialization-helper-growth-limit=1" } */
#include "immutable-inline-context-1.c"
/* { dg-final { scan-ipa-dump-not "Inlining inherited immutable program helper" "inline" } } */
