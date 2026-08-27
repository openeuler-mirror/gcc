/* { dg-do compile } */
/* { dg-options "-fno-ftz" } */

/* The negative spelling was equally real, so it is diagnosed too, and
   points at its own replacement rather than at the positive one.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp ".*error: '-fno-ftz' is no longer supported; use '-mno-daz-ftz' instead" } */
/* { dg-prune-output "confused by earlier errors" } */
