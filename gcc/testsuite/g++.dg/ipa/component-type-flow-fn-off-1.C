/* A function's explicit on option overrides the translation-unit off option.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fno-ipa-icf -fno-component-type-flow -fdump-ipa-devirt-details" } */

#define REVERSE_FUNCTION_ORDER 0
#include "component-type-flow-options.h"

/* { dg-final { scan-ipa-dump-times {       virtual int Popular::value\(\)/[0-9]+ virtual int Uncommon::value\(\)/[0-9]+\n} 2 "devirt" } } */
/* { dg-final { scan-ipa-dump-times "Selecting dominant target \\(4 versus 2 live vtables\\)" 1 "devirt" } } */
/* { dg-final { scan-ipa-dump {speculatively devirtualizing call in int read_on\(Base\*\)/[0-9]+ to virtual int Popular::value\(\)/} "devirt" } } */
/* { dg-final { scan-ipa-dump-not {speculatively devirtualizing call in int read_off\(Base\*\)/} "devirt" } } */
