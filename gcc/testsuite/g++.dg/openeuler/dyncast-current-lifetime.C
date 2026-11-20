/* { dg-do run } */
/* { dg-options "-O2 -fno-component-type-flow -funsafe-dynamic-cast-cache" } */

struct Base;
struct Target;
__attribute__((noipa)) Target *as_target (Base *);
static unsigned transitions;

struct Base
{
  Base ()
  {
    if (as_target (this))
      __builtin_abort ();
    ++transitions;
  }
  virtual ~Base ()
  {
    if (as_target (this))
      __builtin_abort ();
    ++transitions;
  }
};

struct Target : Base
{
  Target ()
  {
    if (as_target (this) != this)
      __builtin_abort ();
    ++transitions;
  }
  ~Target ()
  {
    if (as_target (this) != this)
      __builtin_abort ();
    ++transitions;
  }
};

struct Child : Target
{
  Child ()
  {
    if (as_target (this) != this)
      __builtin_abort ();
    ++transitions;
  }
  ~Child ()
  {
    if (as_target (this) != this)
      __builtin_abort ();
    ++transitions;
  }
};

__attribute__((noipa)) Target *
as_target (Base *p)
{
  return dynamic_cast<Target *> (p);
}

int
main ()
{
  {
    Target target;
    Child child;
    if (as_target (&target) != &target || as_target (&child) != &child
        || as_target (0))
      __builtin_abort ();
  }
  if (transitions != 10)
    __builtin_abort ();
}
