/* { dg-do run } */
/* { dg-options "-O3 -fipa-array-load-inline -fdump-ipa-array-load-inline-details -std=c++11 -fdump-ipa-devirt-details -fdump-ipa-inline-details" } */
/* { dg-additional-sources "inputs/speculative-init.C" } */

#include "inputs/speculative-init.h"
static unsigned tbl[4];
struct derived : base
{
  void init () override
  {
    for (unsigned i = 0; i < 4; ++i)
      tbl[i] = i + 7;
  }
};
static unsigned __attribute__((noinline, noclone, hot))
read_tbl (base *b, unsigned i)
{
  b->init ();
  return tbl[i];
}
int main (int argc, char **)
{
  derived d;
  if (read_tbl (get_other (), 2) != 0 || read_tbl (&d, 2) != 9)
    __builtin_abort ();
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */

/* { dg-final { scan-ipa-dump "1 speculatively devirtualized" "devirt" } } */
/* { dg-final { scan-ipa-dump "Expanding speculative call" "inline" } } */
