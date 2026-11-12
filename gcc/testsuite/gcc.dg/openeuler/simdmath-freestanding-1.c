/* { dg-do compile } */
/* { dg-options "-fsimdmath -ffreestanding" } */

/* A freestanding translation unit does not reserve the library names, so
   the pre-included declarations can only conflict with what it defines -
   they can never help, since -ffreestanding implies -fno-builtin and no
   simd-clone call is generated anyway.  The target's own pre-include has
   always guarded on flag_hosted; this one gained the guard late, after a
   copy of it took only half.  A distro-wide -fsimdmath reaches kernel
   modules and firmware, which is where this bites.

   This runs with the include path in place, which is the only way it
   catches anything: with no header reachable, -ffreestanding changes
   nothing and removing the flag_hosted guard leaves the test passing.
   Measured both ways.  */

int pow;

int
log (int x)
{
  return x + 1;
}

int
f (void)
{
  return pow + log (1);
}
