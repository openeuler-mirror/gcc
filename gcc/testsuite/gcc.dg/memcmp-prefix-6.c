/* { dg-do compile } */
/* { dg-options "-O2 -finline-memcmp-prefix -fdump-rtl-expand-details" } */
/* { dg-final { scan-rtl-dump-not "Expanded ordered first-byte memcmp guard" "expand" } } */
int fixed (const void *a, const void *b) { return __builtin_memcmp(a,b,8); }
int fixed_equal (const void *a, const void *b) { return __builtin_memcmp(a,b,8)==0; }
