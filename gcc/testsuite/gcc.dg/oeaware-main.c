/* { dg-do run { target *-*-linux* *-*-gnu* } }  */
/* { dg-options "-foeaware-policy=1" }  */

int main(void) { return 0; }

/* { dg-final { check-section-exists ".GCC4OE_oeAware" } }  */
/* { dg-final { check-section-content ".GCC4OE_oeAware" "....01000000" } }  */