/* { dg-do compile } */
/* { dg-options "-Os -finline-memcmp-small-equality -fdump-rtl-expand" } */
/* { dg-final { scan-rtl-dump-not "Expanded bounded dynamic equality comparison" "expand" } } */
#include <stddef.h>
int equal (const void *a, const void *b, size_t n)
{ return __builtin_memcmp (a, b, n) == 0; }
