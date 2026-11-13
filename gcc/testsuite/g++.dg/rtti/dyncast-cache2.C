// { dg-do run }
// { dg-options "-O2 -funsafe-dynamic-cast-cache -fdump-tree-gimple" }

// A zero static base offset does not imply that a successful cross-cast
// returns the source address.  Reuse the same cast site after a success.
struct A { virtual ~A () {} };
struct B : A { int b; };
struct X : A { int x; };
struct M : X, B {};

__attribute__((noinline, noclone)) B *
as_b (A *p)
{
  return dynamic_cast<B *> (p);
}

int
main ()
{
  M m;
  A *p = static_cast<X *> (&m);
  B *expected = static_cast<B *> (&m);
  if (as_b (p) != expected || as_b (p) != expected)
    __builtin_abort ();
  B b;
  if (as_b (&b) != &b || as_b (&b) != &b)
    __builtin_abort ();
  if (as_b (p) != expected || as_b (p) != expected || as_b (0))
    __builtin_abort ();
}

// { dg-final { scan-tree-dump "__dcast_cache" "gimple" } }
