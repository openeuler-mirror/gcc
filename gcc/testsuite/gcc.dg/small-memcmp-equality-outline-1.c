/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target non_strict_align } */
/* { dg-options "-O2 -finline-memcmp-small-equality --param=small-memcmp-equality-outline=1 -fdump-rtl-expand-details" } */
#include "memcmp-family-mechanism.h"
/* { dg-final { scan-rtl-dump "Expanded out-of-line equality trial 1" "expand" } } */
/* { dg-final { scan-assembler "__gcc_trial_memcmpeq_bounded" } } */
