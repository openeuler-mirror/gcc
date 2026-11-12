// support if (_150 >= _154)
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

int
main()
{
  arc_p **ap = (arc_p**) malloc(1 * sizeof(arc_p*));
  arc_p **arcs_pointer_sorted = (arc_p**) malloc(1 * sizeof(arc_p*));
  arcs_pointer_sorted[0] = (arc_p*) calloc (1, sizeof(arc_p));

  if (arcs_pointer_sorted >= ap)
    {
      return -1;
    }
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 2" "struct_reorg" } } */
