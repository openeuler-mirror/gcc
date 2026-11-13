/* { dg-do run } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O3 -fchecking=2 -flto -flto-partition=one -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details" } */

static unsigned interpret (const unsigned char *, unsigned)
  __attribute__((optimize("no-tree-immutable-loop-specialization")));
#include "immutable-loop-specialization-2.c"
/* { dg-final { scan-ltrans-tree-dump "Function main" "immutable-unroll1" } } */
/* { dg-final { scan-ltrans-tree-dump-not "Function interpret" "immutable-unroll1" } } */
