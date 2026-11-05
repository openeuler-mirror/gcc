/* { dg-do compile } */
/* { dg-options "-O2 -fno-merge-mull" } */

/* The negative spelling of a removed option.  Without RejectNegative the
   -fno- form decodes to the same option index as the anchor and hit the
   removal diagnostic, so a build carrying -fno-merge-mull failed with an
   error naming an option nobody had written - and that is the spelling
   most likely to have survived, since the feature was default-off from
   2022 and known to miscompile.  It should simply be unrecognized, which
   is also what points the reader at the spelling that explains itself.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp ".*error: unrecognized command-line option '-fno-merge-mull'.*" } */
