/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8 -fdump-tree-immutable-unroll-details" } */
#define MUTABLE
#define main mutable_main
#include "immutable-loop-specialization-6.c"
#undef main
#undef MUTABLE
#undef QUAL
#undef SECOND

/* A reachable immutable control supplies the dump.  The mutable interpreter
   itself must not enter the optional stages; an absent dump alone would not
   demonstrate that distinction.  */
#define main constant_main
#define interpret constant_interpret
#define reference constant_reference
#define a constant_a
#define b constant_b
#define c constant_c
#include "immutable-loop-specialization-1.c"
#undef main
#undef interpret
#undef reference
#undef a
#undef b
#undef c

int
main (void)
{
  mutable_main ();
  constant_main ();
  return 0;
}
/* { dg-final { scan-tree-dump "Function constant_interpret" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump "Peeled loop" "immutable-unroll1" } } */
/* { dg-final { scan-tree-dump-not "Function interpret" "immutable-unroll1" } } */
