/* { dg-do compile } */
/* { dg-options "-ftree-phiprop-disjoint-stores -O2 -fdump-tree-phiprop-details" } */
/* PR tree-optimization/116823 */
/* Make sure the store to *d conflicts with the load from d->a; */


struct s1
{
  int a;
};

void f(int *);

int g(struct s1 i, struct s1 *d, struct s1 *e)
{
  int t3;
  struct s1 t1 = {2};
  const struct s1 t = {10};
  const struct s1 *a;
  {
    int t67;
    f(&t67);
    if (t.a < i.a)
      a = &t;
    else
      a = &i;
  }
  t3 = d->a;
  *d = *a;
  return t3;
}


/* { dg-final { scan-tree-dump-not "Inserting PHI for result of load" "phiprop1"} } */
/* { dg-final { scan-tree-dump-not "Inserting PHI for result of load" "phiprop2"} } */

