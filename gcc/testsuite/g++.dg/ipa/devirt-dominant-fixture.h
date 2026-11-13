/* Each distinct live derived vtable contributes one reference to the
   inherited value method.  Keep two uncommon tables and select the number
   of popular tables in the including test.  */

struct Base
{
  virtual int value () = 0;
};

struct Popular : Base
{
  int value () override { return 1; }
};

struct Popular1 : Popular {};
#if POPULAR_TABLES >= 3
struct Popular2 : Popular {};
#endif
#if POPULAR_TABLES >= 4
struct Popular3 : Popular {};
#endif

struct Uncommon : Base
{
  int value () override { return 2; }
};

struct Uncommon1 : Uncommon {};

Popular popular;
Popular1 popular1;
#if POPULAR_TABLES >= 3
Popular2 popular2;
#endif
#if POPULAR_TABLES >= 4
Popular3 popular3;
#endif
Uncommon uncommon;
Uncommon1 uncommon1;

/* Hide the dynamic type from call-site propagation without hiding either
   virtual target or its live vtables from the inheritance analysis.  */
__attribute__((noipa)) Base *
select_object (bool select_uncommon)
{
  return select_uncommon ? static_cast<Base *> (&uncommon)
                         : static_cast<Base *> (&popular);
}
