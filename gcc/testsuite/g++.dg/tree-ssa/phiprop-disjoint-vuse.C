/* Verify both entry-memory paths and function-specific disable controls.  */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores -fchecking=2 -fdump-tree-phiprop1-details" } */

#define NOIPA __attribute__((noipa))
#define OFF __attribute__((optimize("no-tree-phiprop-disjoint-stores")))

#define SINGLE(NAME, ATTR) \
NOIPA ATTR int NAME (int x, int y, bool choose, int *out) \
{ \
  int *p = choose ? &x : &y; \
  out[0] = 1; \
  return *p; \
}

/* The store in the PHI block has a single dominating entry VUSE.  */
SINGLE (single_on, )
SINGLE (single_off, OFF)

#define VPHI(NAME, ATTR) \
NOIPA ATTR int NAME (int x, int y, bool choose, int *out) \
{ \
  int *p; \
  if (choose) \
    { p = &x; out[0] = x; } \
  else \
    { p = &y; out[0] = y; } \
  out[1] = 1; \
  return *p; \
}

/* Predecessor stores require a virtual PHI.  Edge loads must use its
   corresponding arguments, rather than the PHI result itself.  */
VPHI (vphi_on, )
VPHI (vphi_off, OFF)

int main ()
{
  const int values[] = { -__INT_MAX__ - 1, -17, -1, 0, 1, 17, __INT_MAX__ };
  for (unsigned i = 0; i < sizeof values / sizeof *values; ++i)
    for (unsigned j = 0; j < sizeof values / sizeof *values; ++j)
      for (unsigned c = 0; c != 2; ++c)
        {
          int a[2] = { 0, 0 }, b[2] = { 0, 0 };
          int expected = c ? values[i] : values[j];
          if (single_on (values[i], values[j], c, a) != expected
              || single_off (values[i], values[j], c, b) != expected
              || a[0] != 1 || b[0] != 1)
            __builtin_abort ();
          if (vphi_on (values[i], values[j], c, a) != expected
              || vphi_off (values[i], values[j], c, b) != expected
              || a[0] != expected || b[0] != expected
              || a[1] != 1 || b[1] != 1)
            __builtin_abort ();
        }
}

/* Only the two enabled functions may cross stores.  */
/* { dg-final { scan-tree-dump-times "PHI load propagation: crossed proven-disjoint local stores" 2 "phiprop1" } } */
