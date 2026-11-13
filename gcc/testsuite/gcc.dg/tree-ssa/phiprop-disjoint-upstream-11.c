/* { dg-do compile } */
/* { dg-options "-ftree-phiprop-disjoint-stores -O2 -fno-tree-sra -fdump-tree-phiprop-details" } */
/* PR tree-optimization/123120 */
/* Make sure the load *d conflicts with the store to *d so that phiprop
   does not move the store into conditionals. */

struct s1
{
  int a;
};

void f(int *);

int g(struct s1 i, struct s1 *d, struct s1 *e)
{
  const struct s1 t = {10};
  const struct s1 *a;
  struct s1 t1 = {2};
  if (t.a < i.a)
    a = &t;
  else
    a = &i;
  t1 = *d;
  *d = *a;
  return t1.a;
}

/* { dg-final { scan-tree-dump-not "Inserting PHI for result of load" "phiprop1"} } */
/* { dg-final { scan-tree-dump-not "Inserting PHI for result of load" "phiprop2"} } */

