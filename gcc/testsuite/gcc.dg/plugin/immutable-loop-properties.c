/* { dg-do compile } */
/* { dg-options "-std=gnu11 -O3 -fchecking=2 -fipa-value-specialization -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param max-inline-insns-auto=8 --param max-inline-insns-single=8" } */

/* The plugin checks both early stages actually run, and that subsequent
   consumers are not told that all loop optimizations have finished.  */
#include "../tree-ssa/immutable-loop-specialization-2.c"
