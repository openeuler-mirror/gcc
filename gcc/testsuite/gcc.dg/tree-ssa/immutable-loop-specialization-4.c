/* { dg-do run } */
/* { dg-options "-O3 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details --param immutable-state-peel-limit=8" } */
#include "immutable-loop-specialization-3.c"
/* { dg-final { scan-tree-dump "Bounded immutable state peeling: 8 copies" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump-not "Bounded immutable state peeling: 33 copies" "immutable-unroll1" } } */
