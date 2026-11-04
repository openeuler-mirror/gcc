/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -ftree-phiprop-disjoint-stores -fdump-tree-phiprop2-details" } */
extern void change (int *);
extern void change_volatile (volatile int *);
int clobbered (int x, int y, bool choose)
{
  int *p = choose ? &x : &y;
  change (&x);
  return *p;
}
int volatile_load (int x, int y, bool choose)
{
  volatile int a = x, b = y;
  volatile int *p = choose ? &a : &b;
  change_volatile (&a);
  return *p;
}
int escaped_pointer (int *a, int *b, bool choose)
{
  int *p = choose ? a : b;
  change (a);
  return *p;
}
/* { dg-final { scan-tree-dump-not "PHI load propagation: crossed proven-disjoint local stores" "phiprop2" } } */

