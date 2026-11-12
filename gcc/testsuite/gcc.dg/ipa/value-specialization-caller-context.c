/* { dg-do run } */
/* { dg-options "-O3 -fipa-cp-clone -fipa-value-specialization -fno-inline -fpeel-loops --param ipa-cp-eval-threshold=0 --param max-peeled-insns=1 --param max-completely-peeled-insns=1 -fdump-ipa-cp-details -fdump-ipa-inline-details -fdump-tree-cunroll-details -fdump-tree-optimized" } */
#include "value-specialization-caller-context.h"
/* The caller hint exists even when the specialized calls stay out of line.  */
/* { dg-final { scan-ipa-dump "Recording immutable value context for context_caller" "cp" } } */
/* { dg-final { scan-tree-dump "byte states in context_caller" "cunroll" } } */
/* { dg-final { scan-tree-dump "interpret.constprop" "optimized" } } */
