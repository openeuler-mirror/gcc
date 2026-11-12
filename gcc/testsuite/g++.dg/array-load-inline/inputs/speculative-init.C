#include "speculative-init.h"
struct other : base { void init () override {} };
base *get_other ()
{
  static other o;
  return &o;
}
