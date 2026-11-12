/* Shared guarded/fallback and function-option runtime oracle.  */
#ifndef PROBE_ATTRIBUTE
#define PROBE_ATTRIBUTE
#endif

__attribute__ ((noipa)) PROBE_ATTRIBUTE unsigned
guarded (__SIZE_TYPE__ i, unsigned x)
{
  const unsigned char *p = (const unsigned char *) "abcdef" + i;
  if (*p == 0)
    return x + 1;
  return x + *p;
}

__attribute__ ((noipa)) unsigned
plain_guarded (__SIZE_TYPE__ i, unsigned x)
{
  const unsigned char *p = (const unsigned char *) "abcdef" + i;
  if (*p == 0)
    return x + 1;
  return x + *p;
}

__attribute__ ((noipa, optimize ("no-tree-static-byte-cfg"))) unsigned
reference (__SIZE_TYPE__ i, unsigned x)
{
  const unsigned char *p = (const unsigned char *) "abcdef" + i;
  if (*p == 0)
    return x + 1;
  return x + *p;
}

int
main (void)
{
  for (__SIZE_TYPE__ i = 0; i < sizeof ("abcdef"); ++i)
    for (unsigned x = 0; x < 19; ++x)
      {
	unsigned value = reference (i, x - 7);
	if (guarded (i, x - 7) != value
	    || plain_guarded (i, x - 7) != value)
	  __builtin_abort ();
      }
  return 0;
}
