/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp-clone -fpeel-loops -fno-ipa-value-specialization --param ipa-cp-eval-threshold=0 --param ipa-value-specialization-peel-growth-budget=1 -fdump-tree-cunroll-details" } */

#include "readonly-peel-growth-nested.c"

/* { dg-final { scan-tree-dump-not "IPA readonly peeling growth" "cunroll" } } */
/* { dg-final { scan-tree-dump-not "Considering IPA readonly-value loop" "cunroll" } } */
