/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -finline-memcmp-prefix -fdump-rtl-expand-details -finline-memcmp-small-equality" } */
/* { dg-final { scan-rtl-dump "Expanded ordered first-byte memcmp guard" "expand" } } */
#include "small-memcmp-equality-1.c"
