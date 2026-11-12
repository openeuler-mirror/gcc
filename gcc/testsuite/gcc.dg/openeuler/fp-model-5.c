/* { dg-do compile } */
/* { dg-options "-fp-model=precise" } */

/* The gcc-12 spelling is removed: it must be diagnosed by name and
   point at the supported one, rather than silently doing nothing or
   being mistaken for some other -f option.

   Said once.  dg-error is happy with any number of copies of the message,
   and the option being both Common and Driver did produce two - every
   handler whose mask matches gets called, so a case in
   common_handle_option as well as one in driver_handle_option fires twice
   in the same process.  dg-regexp prunes a single occurrence, so a second
   one is left behind and counts as an excess error.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp ".*error: '-fp-model=precise' is no longer supported; use '-ffp-model=' instead" } */
/* { dg-prune-output "confused by earlier errors" } */
