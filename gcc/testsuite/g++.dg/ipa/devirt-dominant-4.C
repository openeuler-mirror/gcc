/* The exact four-to-two boundary is accepted, with an indirect fallback.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fcomponent-type-flow -fdump-ipa-devirt-details" } */

#define POPULAR_TABLES 4
#include "devirt-dominant-fixture.h"

__attribute__((noinline, noclone)) int
read_value (Base *object)
{
  return object->value ();
}

int
main ()
{
  if (read_value (select_object (false)) != 1
      || read_value (select_object (true)) != 2)
    __builtin_abort ();
}

/* { dg-final { scan-ipa-dump {       virtual int Popular::value\(\)/[0-9]+ virtual int Uncommon::value\(\)/[0-9]+\n} "devirt" } } */
/* { dg-final { scan-ipa-dump-times "Selecting dominant target \\(4 versus 2 live vtables\\)" 1 "devirt" } } */
