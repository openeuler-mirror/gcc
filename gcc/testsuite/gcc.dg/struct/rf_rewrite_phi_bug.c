/* 
Exclude the rewriting error caused by 
first_list_elem = (list_elem *)NULL;
rewriting PHI:first_list_elem_700 = PHI <0B(144), 0B(146)>
into:
first_list_elem.reorder.0_55 = PHI <(144), (146)>
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
  arc_p firstout, firstin;
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
  node_p tail, head;
  short ident;
  arc_p nextout, nextin;
  int64_t flow;
  int64_t org_cost;
};

const int MAX = 100;

list_elem* new_list_elem;
list_elem* first_list_elem;

int
main ()
{
  int i = 0;
  list_elem *first_list_elem;
  list_elem *new_list_elem;
  arc_t *arcout;
  for( ; i < MAX && arcout->ident == -1; i++);

  first_list_elem = (list_elem *)NULL;
  for( ; i < MAX; i++)
    {
      new_list_elem = (list_elem*) calloc(1, sizeof(list_elem));
      new_list_elem->next = first_list_elem;
      first_list_elem = new_list_elem;
    }
  if (first_list_elem != 0)
    {
      return -1;
    }
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 3" "struct_reorg" } } */