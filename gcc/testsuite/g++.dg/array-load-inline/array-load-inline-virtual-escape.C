/* { dg-do run } */
/* { dg-options "-O3 -fno-devirtualize -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */
/* { dg-additional-sources "inputs/virtual-escape.C" } */

#include "inputs/virtual-escape.h"

static unsigned tbl[4];

namespace {
struct Derived : Base
{
  unsigned __attribute__((noinline)) read (unsigned i)
  {
    return tbl[i];
  }
};
}

static void __attribute__((noinline, used))
init ()
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 5;
}

int
main ()
{
  Derived d;
  if (outside (&d) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
