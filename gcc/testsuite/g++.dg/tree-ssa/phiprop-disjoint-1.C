/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -ftree-phiprop-disjoint-stores -fdump-tree-phiprop2-details" } */
template <class T> const T& minref (const T& a, const T& b)
{ return b < a ? b : a; }
template <class T> const T& maxref (const T& a, const T& b)
{ return b > a ? b : a; }
void clamp_pair (const int *p, unsigned n, int seed, int limit, int *out)
{
  int low = seed, high = seed;
  for (unsigned i = 0; i < n; ++i)
    {
      int x = p[i];
      if (x < low) low = x;
      else if (x > high) high = x;
    }
  out[0] = maxref (minref (low, limit), 1);
  out[1] = maxref (minref (high, limit), 1);
}
/* { dg-final { scan-tree-dump "PHI load propagation: crossed proven-disjoint local stores" "phiprop2" } } */

