/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fexpensive-optimizations -ffp-model=except" } */

/* fp-model-34.c with the order reversed: the explicit request comes
   BEFORE the model.  The arbitration is by explicitness, not position -
   the model fills only what the command line left unset - so the
   explicit option must still win from either side.  fp-model-34.c alone
   cannot pin that: with the explicit option written after the model, a
   last-one-wins implementation passes it too.  This pair is what tells
   the two implementations apart.  */

unsigned int
bswap (unsigned int x)
{
  return ((x & 0xffu) << 24) | ((x & 0xff00u) << 8)
	 | ((x >> 8) & 0xff00u) | ((x >> 24) & 0xffu);
}

/* Brace-quoted and anchored on the tabs, as the rest of this suite
   spells mnemonics; see fp-model-34.c.  */
/* { dg-final { scan-assembler {\trev\t} } } */
