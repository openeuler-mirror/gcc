/* { dg-do compile } */

struct foo {int dx; long dy; int dz; };
struct goo {long offset; struct foo* pfoo; };

void* func (long); 

__attribute__((used)) static void
test(struct goo* g)
{
  void* pvoid;
  struct foo* f;

  for (f = g->pfoo; f->dx; f++)
    {
      if (f->dy)
	break;
    }
  f--;

  pvoid = func(f->dz + g->offset);
  return;
}
