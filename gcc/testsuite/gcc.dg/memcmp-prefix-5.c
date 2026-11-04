/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-Os -finline-memcmp-prefix -fdump-rtl-expand-details" } */
/* { dg-final { scan-rtl-dump-not "Expanded ordered first-byte memcmp guard" "expand" } } */
#include "small-memcmp-equality-1.c"
