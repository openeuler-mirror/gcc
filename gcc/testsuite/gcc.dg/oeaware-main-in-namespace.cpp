/* { dg-do compile { target *-*-linux* *-*-gnu* } }  */
/* { dg-options "-foeaware-policy=1" }  */

namespace radar8446940 {
int main () {
 return 0;
}
}

/* { dg-final { scan-assembler-not "GCC4OE_oeAware" } }  */
