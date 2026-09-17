/* { dg-do run } */
/* { dg-options "-O2 -flto -ficp" } */
/* { dg-additional-options "-fdump-ipa-icp-details=./icp9.c.077i.icp" } */

struct type_a
{
  int value;
};

struct type_b
{
  int value;
  int extra;
};

typedef int (*type_a_fn) (struct type_a *);
typedef int (*type_b_fn) (struct type_b *);

type_a_fn volatile fn_a;
type_b_fn volatile fn_b;

__attribute__ ((noinline, noclone))
static int
target_a (struct type_a *p)
{
  return p->value + 1;
}

__attribute__ ((noinline, noclone))
static int
target_b (struct type_b *p)
{
  return p->value + p->extra + 2;
}

__attribute__ ((noinline, noclone))
static void
init_targets (void)
{
  fn_a = target_a;
  fn_b = target_b;
}

__attribute__ ((noinline, noclone))
static int
call_a (struct type_a *p)
{
  return fn_a (p);
}

int
main (void)
{
  struct type_a a = { 1 };
  struct type_b b = { 1, 0 };

  init_targets ();
  if (fn_b (&b) != 3)
    return 1;
  return call_a (&a) != 2;
}

/* Without struct-reorg, ICP must keep using the original precise target
   set.  */
/* { dg-final { scan-ipa-dump-not "ICP-CONSERVATIVE" "icp" } } */
/* { dg-final { scan-ipa-dump "substituted by:.*target_a" "icp" } } */
