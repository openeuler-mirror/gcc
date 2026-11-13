/* Exercise both mechanisms in the same translation unit, without exposing
   the selected dynamic type to interprocedural constant propagation.  */
#define POPULAR_TABLES 4
#include "devirt-dominant-fixture.h"

__attribute__((noinline, noclone)) int
read_value (Base *object)
{
  return object->value ();
}

__attribute__((noinline, noclone)) Popular *
as_popular (Base *object)
{
  return dynamic_cast<Popular *> (object);
}

int
main ()
{
  if (read_value (select_object (false)) != 1
      || read_value (select_object (true)) != 2
      || as_popular (select_object (false)) != &popular
      || as_popular (select_object (false)) != &popular
      || as_popular (select_object (true)) != 0
      || as_popular (0) != 0)
    __builtin_abort ();
}
