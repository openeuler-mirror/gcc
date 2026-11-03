/* { dg-do compile } */
/* { dg-options "-O3 -fno-devirtualize -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

static unsigned tbl[4];

namespace {

struct Base
{
  virtual unsigned read (unsigned) = 0;
};

struct Derived : Base
{
  unsigned __attribute__((noinline)) read (unsigned i)
  {
    return tbl[i];
  }
};

}

void __attribute__((noinline))
init_tbl ()
{
  for (unsigned i = 0; i < 4; ++i)
    tbl[i] = i + 23;
}

unsigned __attribute__((noinline))
call_read (Base *b, unsigned i)
{
  return b->read (i);
}

int
main ()
{
  Derived d;
  unsigned v = call_read (&d, 2);
  init_tbl ();
  return v;
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: load function is externally reachable" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
