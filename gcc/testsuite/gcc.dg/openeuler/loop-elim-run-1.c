/* { dg-do run } */
/* { dg-options "-O2 -floop-elim" } */

/* The rewrite claims the walk always advances.  Check that the optimized
   answer still matches the arithmetic on real data, including the case
   where the first element past base is already zero (one step taken) and
   the case where the walk runs for a while.  */

int __attribute__((noipa))
scan (int *base)
{
  int *p = base + 1;

  while (*p != 0)
    p++;

  return (unsigned int) (p - base) == 0;
}

int main (void)
{
  int a[8] = { 7, 0, 0, 0, 0, 0, 0, 0 };
  int b[8] = { 7, 1, 2, 3, 4, 5, 6, 0 };

  if (scan (a) != 0)
    __builtin_abort ();
  if (scan (b) != 0)
    __builtin_abort ();

  return 0;
}
