/* { dg-options "-O2" } */

float test ();
float g;

float foo (float x, float y) {
  g = x + test ();
  return (x + test ()) * y;
}

/* GCC 12 allocates FP registers in ascending register-number order.  */
/* { dg-final { scan-assembler {\tstp\td8, d9, \[sp,} } } */
/* { dg-final { scan-assembler {\tldp\td8, d9, \[sp,} } } */
