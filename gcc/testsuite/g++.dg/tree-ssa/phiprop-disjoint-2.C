/* { dg-do compile } */
/* { dg-options "-O2 -ftree-phiprop-disjoint-stores -fchecking=2 -fdump-tree-phiprop1-details" } */

/* Require the same pass to perform a legal transformation.  This prevents
   all the rejection checks below from passing when the extension is inert.  */
int disjoint_control (int x, int y, bool choose, int *out)
{
  int *p = choose ? &x : &y;
  *out = 1;
  return *p;
}

extern void change (int *);
extern void change_volatile (volatile int *);

/* The call may change an incoming object: reject in the alias oracle.  */
int clobbered (int x, int y, bool choose)
{
  int *p = choose ? &x : &y;
  change (&x);
  return *p;
}
/* Reject the volatile load before attempting the extension.  */
int volatile_load (int x, int y, bool choose)
{
  volatile int a = x, b = y;
  volatile int *p = choose ? &a : &b;
  change_volatile (&a);
  return *p;
}
/* These PHI inputs are external pointers, not addresses of automatic
   objects.  The ordinary PHI argument check can reject them before the
   extension or its alias queries are reached.  */
int escaped_pointer (int *a, int *b, bool choose)
{
  int *p = choose ? a : b;
  change (a);
  return *p;
}
/* Keep the load in a later block, with a memory state that cannot be used
   on the address PHI's incoming edges.  */
int cross_block (int x, int y, bool choose, bool modify, int *out)
{
  int *p = choose ? &x : &y;
  *out = 1;
  if (modify)
    change (&x);
  return *p;
}

/* { dg-final { scan-tree-dump-times "PHI load propagation: crossed proven-disjoint local stores" 1 "phiprop1" } } */
