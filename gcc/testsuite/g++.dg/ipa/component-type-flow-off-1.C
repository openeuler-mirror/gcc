/* Verify that the explicit off switch preserves the original transforms.  */
/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -fno-component-type-flow -fdump-tree-gimple -fdump-ipa-devirt-details" } */

struct CastBase
{
  virtual ~CastBase () {}
};

struct CastTarget : CastBase {};

__attribute__((noinline)) CastTarget *
cast_target (CastBase *object)
{
  return dynamic_cast<CastTarget *> (object);
}

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

__attribute__((noinline)) int
read_value (Base *object)
{
  return object->value ();
}

int use (CastBase *cast_object, Base *value_object)
{
  return (cast_target (cast_object) != 0) + read_value (value_object);
}

/* { dg-final { scan-tree-dump-not "__dcast_cache" "gimple" } } */
/* { dg-final { scan-ipa-dump-not "Selecting dominant target" "devirt" } } */
