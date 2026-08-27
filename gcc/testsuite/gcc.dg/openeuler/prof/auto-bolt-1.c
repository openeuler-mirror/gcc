/* The AutoBOLT section dump runs after final, on basic-block data the
   pipeline no longer guarantees; with real profile counts a block's
   insn chain can end before BB_END, and the unguarded walk used to
   segfault the compiler on a program this small.  The guarded walkers
   must survive the whole generate-run-use cycle.  */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O2" } */

__attribute__ ((noinline)) int
hot_fn (int x)
{
  return x * 3 + 1;
}

__attribute__ ((noinline)) int
cold_fn (int x)
{
  return x - 7;
}

volatile long sink;

int
main (void)
{
  long s = 0;
  for (int i = 0; i < 200000; i++)
    s += hot_fn (i);
  s += cold_fn (3);
  sink = s;
  return 0;
}
