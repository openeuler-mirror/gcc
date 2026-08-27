/* { dg-do compile } */
/* { dg-options "-O2 -fsimdmath -ffp-model=strict -Wno-simdmath" } */

/* The report has a name, so it can be turned off.  That is the whole
   point of giving it one: without an option index there was no -Wno-,
   and -Werror turned a combination the manual invites into a failed
   build.  Nothing else here asserts the name, so nothing else would go
   red if the index were dropped again.  */

int
main (void)
{
  return 0;
}
