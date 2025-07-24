/* Verify that the function-specific target attribute is honored when
   validating the interrupt ("rnmi") attribute.  The command line does not
   enable Smrnmi, but the functions do.  */
/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */

void __attribute__ ((target ("arch=rv64gc_smrnmi"),
		     interrupt ("rnmi")))
foo (void)
{
}

void __attribute__ ((interrupt ("rnmi"),
		     target ("arch=rv64gc_smrnmi")))
bar (void)
{
}

/* { dg-final { scan-assembler-times {\mmnret} 2 } } */
