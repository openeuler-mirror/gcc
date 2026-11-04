/* { dg-do compile } */
/* { dg-options "-O2 -finline-memcmp-prefix -fnon-call-exceptions -fdump-rtl-expand-details" } */
/* { dg-final { scan-rtl-dump-not "Expanded ordered first-byte memcmp guard" "expand" } } */
#include "small-memcmp-equality-2.c"
