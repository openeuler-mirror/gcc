/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -ffp-model=except -fexpensive-optimizations" } */

/* The control for fp-model-33.c: what the model turns off, an explicit
   request turns back on.  Without this, fp-model-33.c would pass against
   a compiler that had simply stopped recognizing byte swaps.

   The explicit option is written after the model here, so this test
   alone cannot distinguish explicitness-based arbitration from
   last-one-wins; fp-model-37.c is the order-reversed twin that can, and
   the two must be read as a pair.  */

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
/* { dg-final { scan-assembler {\trev\t} } } */
