/* { dg-do run } */
/* { dg-options "-O3 -fipa-value-specialization -ftree-immutable-loop-specialization --param ipa-value-specialization-peel-growth-budget=0 --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details" } */

/* The additional tree stages must retain the function-wide readonly growth
   bound as well as their own per-stage bound.  */
#include "immutable-loop-specialization-1.c"

/* { dg-final { scan-tree-dump "IPA readonly peeling growth budget exhausted" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump-not "Bounded immutable state peeling" "immutable-unroll1" } } */
