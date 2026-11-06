/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details" } */
#define DIFFERENT
#include "immutable-loop-specialization-6.c"
/* { dg-final { scan-tree-dump "Function interpret" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump-not "Pointer PHI shares one constant-address object" "immutable-unroll1" } } */
