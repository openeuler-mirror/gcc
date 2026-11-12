/* Verify that interrupt ("rnmi") requires the Smrnmi extension.  */
/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */

void __attribute__ ((interrupt ("rnmi")))
foo (void)
{ /* { dg-error "requires the Smrnmi ISA extension" } */
}
