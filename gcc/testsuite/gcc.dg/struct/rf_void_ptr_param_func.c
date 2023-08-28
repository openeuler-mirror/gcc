// Add a safe func mechanism.
// avoid escape_unkown_field, "Type escapes via an unkown field accessed"
// avoid escape_cast_void, "Type escapes a cast to/from void*" eg: GIMPLE_NOP
/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

typedef struct arc arc_t;
typedef struct arc *arc_p;

struct arc
{
  int id;
  int64_t cost;
  short ident;
  arc_p nextout;
  arc_p nextin;
  int64_t flow;
  int64_t org_cost;
};

void
__attribute__((noinline)) spec_qsort (void *a, size_t es) 
{
  char *pa;
  char *pb;
  int cmp_result;

  while ((*(arc_t **)a)->id < *((int *)a))
    { 
      if (cmp_result == 0)
	{
	  spec_qsort (a, es);
	  pa = (char *)a - es;
	  a += es;
	  *(long *)pb = *(long *)pa;
	}
      else
	{
	  a -= pa - pb;
	}
    }  
}

int
main()
{
  arc_p **arcs_pointer_sorted;
  spec_qsort (arcs_pointer_sorted[0], sizeof (arc_p));
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "reorder_fields" } } */