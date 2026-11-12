#include "virtual-escape.h"

extern "C" unsigned
outside (Base *p)
{
  return p->read (2);
}
