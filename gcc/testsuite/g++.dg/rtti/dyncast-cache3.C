// { dg-do run { target c++20 } }
// { dg-options "-O2 -fcomponent-type-flow -fdump-tree-gimple" }

// Preserve constant evaluation both in constexpr-capable functions and in
// initializers/static assertions outside such functions.
struct A { virtual int f () const { return 0; } };
struct B : A {};
struct C : A {};

__attribute__((noinline, noclone)) constexpr const B *
as_b (const A *p)
{
  return dynamic_cast<const B *> (p);
}

constexpr B b;
constexpr C c;
constexpr const A *a = &b;
constexpr const B *p = dynamic_cast<const B *> (a);
static_assert (p == &b);
static_assert (as_b (&b) == &b);
static_assert (as_b (&c) == nullptr);
static_assert (as_b (nullptr) == nullptr);
static_assert ([] { B b; const A *a = &b;
                    return dynamic_cast<const B *> (a) == &b; } ());

void
non_constexpr ()
{
  constexpr const B *q = dynamic_cast<const B *> (a);
  static_assert (q == &b);
  static_assert (dynamic_cast<const B *> (a) == &b);
}

int
main ()
{
  if (as_b (&b) != &b || as_b (&b) != &b
      || as_b (&c) != nullptr || as_b (nullptr) != nullptr)
    __builtin_abort ();
}

// { dg-final { scan-tree-dump "__dcast_cache" "gimple" } }