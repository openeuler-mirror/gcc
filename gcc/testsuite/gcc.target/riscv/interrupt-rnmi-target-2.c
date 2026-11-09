/* Verify that interrupt ("rnmi") is rejected when the function-specific
   target attribute disables Smrnmi, instead of triggering an internal
   compiler error on the missing mnret instruction.  */
/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-march=rv64gc_smrnmi -mabi=lp64" } */

void __attribute__ ((target ("arch=rv64gc"),
		     interrupt ("rnmi")))
foo (void)
{ /* { dg-error "requires the Smrnmi ISA extension" } */
}

void __attribute__ ((interrupt ("rnmi"),
		     target ("arch=rv64gc")))
bar (void)
{ /* { dg-error "requires the Smrnmi ISA extension" } */
}
