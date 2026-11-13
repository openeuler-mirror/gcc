/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details" } */

static unsigned interpret (const unsigned char *, unsigned)
  __attribute__((optimize("no-tree-immutable-loop-specialization")));
#include "immutable-loop-specialization-2.c"
/* The caller supplies an actually executed stage, while the function-level
   option keeps the interpreter and its clones out of that stage.  */
/* { dg-final { scan-tree-dump "Function main" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump-not "Function interpret" "immutable-unroll1" } } */
