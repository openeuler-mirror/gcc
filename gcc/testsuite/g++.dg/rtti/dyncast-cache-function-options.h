struct A { virtual ~A () {} };
struct B : A {};
struct C : A {};

__attribute__((noinline, noclone, optimize (CACHE_OPTIONS))) B *
as_b (A *p)
{
  return dynamic_cast<B *> (p);
}

int
main ()
{
  B b;
  C c;
  if (as_b (&b) != &b || as_b (&b) != &b
      || as_b (&c) != 0 || as_b (0) != 0)
    __builtin_abort ();
}
