/* { dg-do compile } */
/* { dg-options "-fftz" } */

/* The old spelling is removed.  It was a common option that only ever did
   anything on AArch64, and upstream already calls the identical thing
   -mdaz-ftz on x86 - the AArch64 control bit covers what x86 splits
   between FTZ and DAZ - so the option is now spelled that way here too.
   A stale command line is told the new name rather than being met with
   "unrecognized command-line option".  */

int
main (void)
{
  return 0;
}

/* { dg-regexp ".*error: '-fftz' is no longer supported; use '-mdaz-ftz' instead" } */
/* { dg-prune-output "confused by earlier errors" } */
