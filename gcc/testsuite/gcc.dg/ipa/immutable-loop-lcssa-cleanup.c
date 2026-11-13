/* { dg-do run } */
/* { dg-options "-fno-ipa-value-specialization-clone-budget -fno-ipa-value-specialization-loop -fno-ipa-value-specialization-shared-loop -fno-ipa-value-specialization-cold -fno-ipa-value-specialization-helper-expansion -O3 -fchecking=2 -fipa-value-specialization -fipa-value-specialization-loop -ftree-immutable-loop-specialization --param early-inlining-insns=0 --param ipa-cp-eval-threshold=10000000" } */
/* Exercise scalar cleanup after optional immutable peeling, including values
   carried out of loops and data-dependent interpreter state.  Checking must
   not impose loop-closed SSA on ordinary scalar propagation.  */
#include "immutable-loop-value-1.c"
