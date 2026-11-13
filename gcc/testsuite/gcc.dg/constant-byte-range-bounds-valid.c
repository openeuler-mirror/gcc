/* A negative MEM_REF displacement can still designate an in-bounds byte.  */
/* { dg-do run } */
/* { dg-options "-O2 -Warray-bounds -ftree-constant-byte-range -fdump-tree-constbyterange-details" } */

__attribute__ ((noipa)) int
previous (unsigned n)
{
  if (n < 1 || n > 4)
    return -1;
  const char *p = "abc" + n;
  return p[-1] == '@';
}

__attribute__ ((noipa)) int
next (unsigned n)
{
  if (n > 2)
    return -1;
  const char *p = "abc" + n;
  return p[1] == '@';
}

int
main (void)
{
  for (unsigned n = 0; n != 6; ++n)
    if (previous (n) != ((n >= 1 && n <= 4) ? 0 : -1)
	|| next (n) != (n <= 2 ? 0 : -1))
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Constant byte object range" 2 "constbyterange1" } } */
