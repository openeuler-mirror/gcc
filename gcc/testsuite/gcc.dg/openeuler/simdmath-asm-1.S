/* { dg-do assemble } */
/* { dg-options "-fsimdmath" } */

/* -fsimdmath must not pre-include anything into assembler input.  The
   header is C; the assembler reads its eleven declarations as
   instructions and rejects every one of them:

     simdmath.h: Assembler messages:
     simdmath.h:28: Error: unknown mnemonic `double' -- `double cos(double x)'

   which means a project that puts -fsimdmath in its global CFLAGS - the
   intended use - fails to build every .S file it has.  The CLK_ASM guard
   is what prevents that, and it had no test: openeuler.exp's glob has
   always accepted *.S, but the directory contained none.  */

	.text
	.globl	simdmath_asm_probe
	.type	simdmath_asm_probe, %function
simdmath_asm_probe:
	ret
	.size	simdmath_asm_probe, .-simdmath_asm_probe
