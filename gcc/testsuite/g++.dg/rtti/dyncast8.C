// Verify dynamic_cast through a public non-virtual single-inheritance chain,
// including the general-RTTI fallbacks used for other hierarchy shapes.
// { dg-do run { target c++11 } }
// { dg-options "-O2 -fcomponent-type-flow" }

struct A
{
  virtual ~A() {}
};

struct B : A
{
  int b = 1;
};

struct C : B
{
  int c = 2;
};

struct D : A
{
  int d = 3;
};

struct L
{
  virtual ~L() {}
};

struct R
{
  virtual ~R() {}
};

struct M : L, R
{
  int m = 4;
};

int
main()
{
  C c;
  A *a = &c;
  if (dynamic_cast<B *> (a) != static_cast<B *> (&c))
    __builtin_abort ();
  if (dynamic_cast<C *> (a) != &c)
    __builtin_abort ();

  D d;
  a = &d;
  if (dynamic_cast<B *> (a) != 0)
    __builtin_abort ();

  M m;
  L *l = &m;
  R *r = dynamic_cast<R *> (l);
  if (r != static_cast<R *> (&m))
    __builtin_abort ();
  if (dynamic_cast<M *> (r) != &m)
    __builtin_abort ();
  if (dynamic_cast<C *> (l) != 0)
    __builtin_abort ();
}
