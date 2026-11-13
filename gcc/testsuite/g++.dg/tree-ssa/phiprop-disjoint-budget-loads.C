/* Stop repeated scans after some loads of one PHI have been replaced,
   without changing the remaining loads.  */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores --param=phiprop-disjoint-stores-max-work=128 -fchecking=2 -fdump-tree-phiprop1-details" } */

__attribute__((noipa)) int many_loads (int a, int b, bool choose, int *out)
{
  int *p = choose ? &a : &b;
  out[0] = 1;
#define STORE(N) out[N] = *p;
  STORE (1) STORE (2) STORE (3) STORE (4)
  STORE (5) STORE (6) STORE (7) STORE (8)
  STORE (9) STORE (10) STORE (11) STORE (12)
  STORE (13) STORE (14) STORE (15) STORE (16)
  return *p;
}

int main ()
{
  int out[17];
  for (unsigned c = 0; c != 2; ++c)
    {
      int expected = c ? -__INT_MAX__ - 1 : __INT_MAX__;
      if (many_loads (-__INT_MAX__ - 1, __INT_MAX__, c, out) != expected
          || out[0] != 1)
        __builtin_abort ();
      for (unsigned i = 1; i != 17; ++i)
        if (out[i] != expected)
          __builtin_abort ();
    }
}

/* { dg-final { scan-tree-dump-times "PHI load propagation: crossed proven-disjoint local stores" 1 "phiprop1" } } */
/* { dg-final { scan-tree-dump-times "PHI load propagation: disjoint-store work budget exhausted" 1 "phiprop1" } } */
