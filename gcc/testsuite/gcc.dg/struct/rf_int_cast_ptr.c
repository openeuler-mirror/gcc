// release escape_cast_another_ptr, "Type escapes a cast to a different pointer"
/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

typedef struct node node_t;
typedef struct node *node_p;

typedef struct arc arc_t;
typedef struct arc *arc_p;

struct node
{
  int64_t potential;
  int orientation;
  node_p child;
  node_p pred;
  node_p sibling;
  node_p sibling_prev;
  arc_p basic_arc;
  arc_p firstout;
  arc_p firstin;
  arc_p arc_tmp;
  int64_t flow;
  int64_t depth;
  int number;
  int time;
};

struct arc
{
  int id;
  int64_t cost;
  node_p tail;
  node_p head;
  short ident;
  arc_p nextout;
  arc_p nextin;
  int64_t flow;
  int64_t org_cost;
};

typedef int cmp_t(const void *, const void *);

__attribute__((noinline)) void
spec_qsort(void *a, cmp_t *cmp)
{
  char *pb = NULL;
  while (cmp(pb, a))
    {
      pb += 1;
    }
}

static int arc_compare( arc_t **a1, int a2 )
{
  if( (*a1)->id < a2 )
    {
      return -1;
    }
  return 1;
}

int
main()
{
  spec_qsort(NULL, (int (*)(const void *, const void *))arc_compare);
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "struct_reorg" } } */