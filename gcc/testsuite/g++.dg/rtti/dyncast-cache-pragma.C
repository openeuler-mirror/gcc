/* The cache option must be restored independently of component type-flow.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fcomponent-type-flow -fdump-tree-gimple" } */
struct A { virtual ~A () {} };
struct B : A {};
struct C : A {};

#pragma GCC push_options
#pragma GCC optimize ("unsafe-dynamic-cast-cache")
__attribute__((noinline, noclone)) B *
cached (A *p)
{
  return dynamic_cast<B *> (p);
}
#pragma GCC pop_options

__attribute__((noinline, noclone)) B *
not_cached (A *p)
{
  return dynamic_cast<B *> (p);
}

int
main ()
{
  B b;
  C c;
  if (cached (&b) != &b || cached (&b) != &b
      || cached (&c) != 0 || cached (0) != 0
      || not_cached (&b) != &b || not_cached (&c) != 0
      || not_cached (0) != 0)
    __builtin_abort ();
}

/* Only cached writes the two TLS slots.  */
/* { dg-final { scan-tree-dump-times {__dcast_cache\.[0-9]+ = } 2 "gimple" } } */
