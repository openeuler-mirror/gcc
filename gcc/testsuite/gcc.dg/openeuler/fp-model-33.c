/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -ffp-model=except" } */

/* -ffp-model=except and strict turn off -fexpensive-optimizations, and
   that had no test: the flag was recorded as unobservable, on the
   grounds that no stable case could be built for it.  One can.
   pass_optimize_bswap is gated on it directly, and on AArch64 a byte
   reversal it recognizes is one rev instruction.  */

unsigned int
bswap (unsigned int x)
{
  return ((x & 0xffu) << 24) | ((x & 0xff00u) << 8)
	 | ((x >> 8) & 0xff00u) | ((x >> 24) & 0xffu);
}

/* Brace-quoted and anchored on the tabs, as the rest of this suite
   spells mnemonics.  \b is a backspace in Tcl regexps, not a word
   boundary, so "\\brev\\b" matches nothing at all - which a
   scan-assembler-not passes vacuously.  */
/* { dg-final { scan-assembler-not {\trev\t} } } */
