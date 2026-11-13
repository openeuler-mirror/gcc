/* Both functions have the same candidate set.  Reversing their definitions
   exercises both traversal orders, including an off-policy rejection before
   an enabled query of the same cached target list.  */

#define POPULAR_TABLES 4
#include "devirt-dominant-fixture.h"

#if REVERSE_FUNCTION_ORDER
__attribute__((noinline, noclone, optimize("component-type-flow"))) int
read_on (Base *object)
{
  return object->value ();
}

__attribute__((noinline, noclone, optimize("no-component-type-flow"))) int
read_off (Base *object)
{
  return object->value ();
}
#else
__attribute__((noinline, noclone, optimize("no-component-type-flow"))) int
read_off (Base *object)
{
  return object->value ();
}

__attribute__((noinline, noclone, optimize("component-type-flow"))) int
read_on (Base *object)
{
  return object->value ();
}
#endif

int
main ()
{
  if (read_on (select_object (false)) != 1
      || read_on (select_object (true)) != 2
      || read_off (select_object (false)) != 1
      || read_off (select_object (true)) != 2)
    __builtin_abort ();
}
