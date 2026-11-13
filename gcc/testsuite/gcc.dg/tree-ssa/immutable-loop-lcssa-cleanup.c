/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -fipa-value-specialization -ftree-immutable-loop-specialization -fno-tree-immutable-loop-specialization-cursor --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details" } */
/* Scalar cleanup must not inherit loop-closed SSA requirements.  */
/* { dg-final { scan-tree-dump "Bounded immutable state peeling" "immutable-unroll1" } } */
#include "immutable-loop-specialization-1.c"
