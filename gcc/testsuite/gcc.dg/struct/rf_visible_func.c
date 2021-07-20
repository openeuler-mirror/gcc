// release escape_visible_function, "Type escapes via expternally visible function call"
// compile options: gcc -O3 -fno-inline -fwhole-program 
// -flto-partition=one -fipa-struct-reorg arc_compare.c -fdump-ipa-all -S -v
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

__attribute__((noinline)) static int
arc_compare( arc_t **a1, arc_t **a2 )
{
  if( (*a1)->flow > (*a2)->flow )
    {
      return 1;
    }
  if( (*a1)->flow < (*a2)->flow )
    {
      return -1;
    }
  if( (*a1)->id < (*a2)->id )
    {
      return -1;
    }

    return 1;
}

__attribute__((noinline)) void
spec_qsort(void *array, int nitems, int size,
	   int (*cmp)(const void*,const void*))
{
  for (int i = 0; i < nitems - 1; i++)
  {
    if (cmp (array , array))
      {
        printf ("CMP 1\n");
      }
    else
      {
        printf ("CMP 2\n");
      }
  }
}

typedef int cmp_t(const void *, const void *);

int
main ()
{
  void *p = calloc (100, sizeof (arc_t **));
  spec_qsort (p, 100, 0, (int (*)(const void *, const void *))arc_compare);
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 2" "reorder_fields" } } */