/* { dg-do compile } */
/* { dg-options "-O2 -fmerge-mull" } */

/* The option that used to enable the match.pd implementation is gone.  It
   is diagnosed by name and points at what replaced it, rather than
   reaching the driver's generic unrecognized-option path.

   Once: the option is Common Driver, and handle_option calls every
   handler whose mask matches, so a case in common_handle_option as well
   as one in driver_handle_option reports twice in the same process.
   dg-error accepts any number of copies, dg-regexp prunes exactly one
   and leaves the rest to count as excess errors.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp ".*error: '-fmerge-mull' is no longer supported; the transformation it enabled is part of '-mmul-widen128' on AArch64" } */
/* { dg-prune-output "confused by earlier errors" } */
