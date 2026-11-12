extern unsigned public_get (unsigned), public_run (unsigned);

int
main (void)
{
  unsigned a = public_get (2);
  unsigned b = public_run (2);
  if (a != 0 || b != 7)
    __builtin_abort ();
  return 0;
}
