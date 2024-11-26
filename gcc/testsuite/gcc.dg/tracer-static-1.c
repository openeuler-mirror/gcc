/* { dg-do compile } */
/* { dg-options "-O3 -ftracer-static -fdump-tree-tracer" } */

static __attribute__ ((noinline)) int fib (int n)
{
  if (n < 3)
    return 0;

  long long fib1 = 0, fib2 = 1;
  long long currentFib = 0;

  for (int i = 3; i <= n; ++i)
    {
      currentFib = fib1 + fib2;
      fib1 = fib2;
      fib2 = currentFib;
    }

  return currentFib;
}

int main (int argc, char** argv)
{
  int n = argc;
  return fib (n);
}

/* { dg-final { scan-tree-dump-times "BB\\d+ with n = \\d+ will not be covered by tracer formation" 4 "tracer" } } */