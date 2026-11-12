/* { dg-do compile } */
/* { dg-options "-O2 -fdebruijn-clz -fchecking=2" } */
#include <setjmp.h>
typedef unsigned int U32;
typedef unsigned long long U64;
static inline unsigned
ctz32f (U32 val)
{
static const U32 table[32] = {
  0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
  31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9 };
return table[((U32) ((val & ((~val) + 1)) * 0x077CB531U)) >> 27];
}
jmp_buf env;
void log_count (unsigned);
unsigned
f (U64 v)
{
U32 lo = (U32) v, hi = (U32) (v >> 32);
unsigned r;
if (lo == 0)
  r = 32 + ctz32f (hi);
else
  r = ctz32f (lo);
log_count (r);
if (setjmp (env))
  return r;
log_count (r);
r++;
log_count (r);
return r;
}
