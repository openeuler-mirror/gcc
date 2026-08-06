// { dg-do run }
// { dg-options "-O2 -fcomponent-type-flow -fdump-tree-gimple" }

struct A
{
  virtual ~A () {}
};

struct B : A
{
  int value = 1;
};

struct C : A
{
  int value = 2;
};

__attribute__((noinline)) B *
as_b (A *p)
{
  return dynamic_cast<B *> (p);
}

int
main ()
{
  B b;
  C c;
  if (as_b (&b) != &b)
    __builtin_abort ();
  if (as_b (&b) != &b)
    __builtin_abort ();
  if (as_b (&c) != 0)
    __builtin_abort ();
  if (as_b (0) != 0)
    __builtin_abort ();
}

// { dg-final { scan-tree-dump "__dcast_cache" "gimple" } }
