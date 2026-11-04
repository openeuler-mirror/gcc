/* Verify that a virtual target shared by a dominant number of live vtables is
   selected speculatively, while the uncommon target still uses the fallback.  */
/* { dg-do run { target c++11 } } */
/* { dg-options "-O2 -fcomponent-type-flow -fdump-ipa-devirt-details" } */

struct Base
{
  virtual int value () = 0;
  virtual ~Base () {}
};

struct Popular : Base
{
  int value () override { return 1; }
};

struct Popular1 : Popular {};
struct Popular2 : Popular {};
struct Popular3 : Popular {};

struct Uncommon : Base
{
  int value () override { return 2; }
};

Popular popular;
Popular1 popular1;
Popular2 popular2;
Popular3 popular3;
Uncommon uncommon;

__attribute__((noipa)) Base *
select_object (bool select_uncommon)
{
  return select_uncommon ? static_cast<Base *> (&uncommon)
                         : static_cast<Base *> (&popular);
}

__attribute__((noinline)) int
read_value (Base *object)
{
  return object->value ();
}

int main ()
{
  return read_value (select_object (true)) != 2;
}

/* { dg-final { scan-ipa-dump "Selecting dominant target \\(4 versus 1 live vtables\\)" "devirt" } } */
