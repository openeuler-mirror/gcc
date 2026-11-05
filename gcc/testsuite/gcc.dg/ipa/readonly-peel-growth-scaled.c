/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp-clone -fpeel-loops -fipa-value-specialization --param ipa-cp-eval-threshold=0 --param ipa-value-specialization-peel-growth-budget=256 -fdump-tree-cunroll-details -fdump-tree-optimized" } */

#include "readonly-peel-growth-siblings.c"

/* A single snapshot of the initial function cost permits both small
   sibling loops, without replenishing the budget after the first peel.  */
/* { dg-final { scan-tree-dump {IPA readonly peeling budget: [0-9]+ units \(initial function cost [0-9]+, growth 800%\)} "cunroll" } } */
/* { dg-final { scan-tree-dump-times "IPA readonly peeling budget:" 2 "cunroll" } } */
/* { dg-final { scan-tree-dump-not "IPA readonly peeling growth budget exhausted" "cunroll" } } */
/* Both clones contain seven copies of three calls in each of two loops.  */
/* { dg-final { scan-tree-dump-times {observe_byte \(} 84 "optimized" } } */
