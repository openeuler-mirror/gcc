/* { dg-do compile } */
/* { dg-options "-fno-fast-math -ffp-model=fast" } */

/* The other order: the model comes last and wins, silently.  Together
   with fp-model-11.c this pins that the cancellation is about order, not
   about the model always losing to an explicit flag.  */
#ifndef __FAST_MATH__
#error -ffp-model=fast after -fno-fast-math did not take effect
#endif

int
main (void)
{
  return 0;
}
