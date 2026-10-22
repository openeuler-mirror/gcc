/* PR tree-optimization/109462 */
/* { dg-do run } */

struct range { int start, end; };

__attribute__((noipa)) int
test (const struct range *ranges, int count)
{
  int last_end;

  for (int i = 0; i < count; i++)
    {
      int start = ranges[i].start;
      int end = ranges[i].end;

      if (start >= end)
	return -22;
      if (i > 0 && last_end > start)
	return -22;
      last_end = end;
    }
  return 0;
}

int
main (void)
{
  const struct range r[] = { { 1, 2 }, { 3, 4 } };
  if (test (r, 2) != 0)
    __builtin_abort ();
  return 0;
}
