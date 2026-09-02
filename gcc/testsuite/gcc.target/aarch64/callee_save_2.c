/* { dg-options "-O2 -fomit-frame-pointer" } */

int test (int x), test2 (int x);

int foo (int x, int y) {
    test (x);
    int lhs = test2 (y);
    return x + lhs;
}

/* { dg-final { scan-assembler {\tstp\tx30, x19, \[sp,} } } */
/* { dg-final { scan-assembler {\tldp\tx30, x19, \[sp\],} } } */
/* GCC 12 expands incoming arguments through additional pseudos, so the
   second value does not retain a profitable preference for x1.  */
/* { dg-final { scan-assembler {\tstr\tx20, \[sp,} } } */
/* { dg-final { scan-assembler {\tldr\tx20, \[sp,} } } */
