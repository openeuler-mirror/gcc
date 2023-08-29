// rewrite_cond bugfix；
/*
if (iterator_600 != 0B)
old rewrite: _1369 = iterator.reorder.0_1249 != 0B; if (_1369 != 1)
new rewrite: if (iterator.reorder.0_1249 != 0B)
*/
/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

typedef struct node node_t;
typedef struct node *node_p;

typedef struct arc arc_t;
typedef struct arc *arc_p;

typedef struct list_elem
{
  arc_t* arc;
  struct list_elem* next;
}list_elem;

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

int i = 0;

int
main ()
{
  register list_elem *first_list_elem;
  register list_elem* iterator;
  iterator = first_list_elem->next;
  while (iterator)
    {
      iterator = iterator->next;
      i++;
    }

  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 3" "struct_reorg" } } */