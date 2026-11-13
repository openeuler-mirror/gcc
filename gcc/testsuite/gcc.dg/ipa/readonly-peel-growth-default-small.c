/* Short independent loops must retain genuine default-budget peeling.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fchecking=2 -fipa-cp-clone -fpeel-loops -fipa-value-specialization --param ipa-cp-eval-threshold=0 -fdump-tree-cunroll-details -fdump-tree-optimized" } */

#include "readonly-peel-growth-siblings.c"

/* { dg-final { scan-tree-dump "IPA readonly peeling growth:" "cunroll" } } */
/* { dg-final { scan-tree-dump-not "IPA readonly peeling growth budget exhausted" "cunroll" } } */
/* { dg-final { scan-tree-dump-times {observe_byte \(} 84 "optimized" } } */
