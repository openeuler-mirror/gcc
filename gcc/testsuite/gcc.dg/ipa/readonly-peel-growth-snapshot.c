/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp-clone -fpeel-loops -fipa-value-specialization --param ipa-cp-eval-threshold=0 --param ipa-value-specialization-peel-growth-budget=1 --param ipa-value-specialization-peel-growth-percent=10000 -fdump-tree-cunroll-details -fdump-tree-optimized" } */

#include "readonly-peel-growth-nested.c"

/* The initial function is small.  Recomputing its relative allowance after
   expanding the inner loop would incorrectly admit the much larger outer
   duplication.  Freeze the allowance independently for each clone.  */
/* { dg-final { scan-tree-dump-times "IPA readonly peeling budget:" 2 "cunroll" } } */
/* { dg-final { scan-tree-dump "IPA readonly peeling growth:" "cunroll" } } */
/* { dg-final { scan-tree-dump "IPA readonly peeling growth budget exhausted" "cunroll" } } */
/* { dg-final { scan-tree-dump-times {observe_byte \(} 378 "optimized" } } */
