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
    tbl[i] = i + 19;
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
  init_tbl ();
  return call_read (&d, 2) != 21;
}

/* { dg-final { scan-ipa-dump "array-load-inline: candidate array tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: reject tbl: init does not dominate load" "array-load-inline" } } */
