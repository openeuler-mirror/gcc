/* { dg-do compile { target *-*-linux* *-*-gnu* } }  */
/* { dg-options "-foeaware-policy=1" }  */

int test(void) { return 0; }

/* { dg-final { scan-assembler-not "GCC4OE_oeAware" } }  */