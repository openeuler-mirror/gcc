/* { dg-do run { target { *-*-linux* } } } */
/* { dg-options "-O2 -fno-component-type-flow -funsafe-dynamic-cast-cache -Wl,--wrap=__dynamic_cast -fdump-tree-gimple" } */

/* An exact current type, a short single-base chain or the primary base
   of a multiple-inheritance object can avoid the runtime search.  */
struct Base { virtual ~Base () {} };
struct Target : Base {};
struct Child : Target {};
struct Grandchild : Child {};
struct Deeper : Grandchild {};
struct Other : Base {};
struct Prefix : Base {};
struct Multiple : Prefix, Target {};
struct Primary : Target, Prefix {};
struct Public : Base {};
struct Secret : Base {};
struct Mixed : Public, private Secret
{
  Base *private_base () { return static_cast<Secret *> (this); }
};

static unsigned calls;
extern "C" void *__real___dynamic_cast (const void *, const void *,
					 const void *, __PTRDIFF_TYPE__);
extern "C" void *
__wrap___dynamic_cast (const void *p, const void *source,
		      const void *target, __PTRDIFF_TYPE__ offset)
{
  ++calls;
  return __real___dynamic_cast (p, source, target, offset);
}

__attribute__((noipa)) Target *
as_target (Base *p)
{
  return dynamic_cast<Target *> (p);
}

__attribute__((noipa)) Mixed *
as_mixed (Base *p)
{
  return dynamic_cast<Mixed *> (p);
}

#include <cxxabi.h>

int
main ()
{
  Target target;
  Child child;
  Grandchild grandchild;
  Deeper deeper;
  Other other;
  Multiple multiple;
  Primary primary;
  Mixed mixed;
  if (as_target (&target) != &target || as_target (&target) != &target
      || as_target (0) != 0 || calls != 0)
    __builtin_abort ();
  if (as_target (&child) != &child || as_target (&grandchild) != &grandchild
      || calls != 0)
    __builtin_abort ();
  if (as_target (&deeper) != &deeper || calls != 1)
    __builtin_abort ();
  if (as_target (&other) != 0 || calls != 2)
    __builtin_abort ();
  Base *cross = static_cast<Prefix *> (&multiple);
  if (as_target (cross) != static_cast<Target *> (&multiple) || calls != 3)
    __builtin_abort ();
  if (as_target (static_cast<Target *> (&primary)) != &primary || calls != 3)
    __builtin_abort ();
  Base *secondary = static_cast<Prefix *> (&primary);
  if (as_target (secondary) != &primary || calls != 4)
    __builtin_abort ();
  /* The hint ignores private paths.  Exact RTTI alone must not turn the
     private, nonzero-offset Base into a successful downcast.  */
  if (as_mixed (static_cast<Public *> (&mixed)) != &mixed || calls != 4)
    __builtin_abort ();
  if (as_mixed (mixed.private_base ()) != 0 || calls != 5)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump {== &_ZTI6Target} "gimple" } } */
/* { dg-final { scan-tree-dump {__dynamic_cast \(} "gimple" } } */
/* { dg-final { scan-tree-dump-not "__dcast_cache" "gimple" } } */
