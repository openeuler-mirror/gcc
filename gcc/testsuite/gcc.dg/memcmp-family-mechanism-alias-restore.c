/* { dg-do compile { target { aarch64*-*-* x86_64-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target non_strict_align } */
/* { dg-options "-O2 -finline-memcmp -fno-inline-memcmp-small-equality -finline-small-memcmp-equality -fdump-rtl-expand-details" } */
#include "memcmp-family-mechanism.h"
/* { dg-final { scan-rtl-dump "Expanded bounded dynamic equality comparison" "expand" } } */
/* { dg-final { scan-rtl-dump "Expanded ordered first-byte memcmp guard" "expand" } } */
