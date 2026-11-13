/* A three-to-two plurality is below the two-to-one dominance threshold.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fcomponent-type-flow -fdump-ipa-devirt-details" } */

#define POPULAR_TABLES 3
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
/* { dg-final { scan-ipa-dump-times "More than one likely target" 1 "devirt" } } */
/* { dg-final { scan-ipa-dump-not "Selecting dominant target" "devirt" } } */
