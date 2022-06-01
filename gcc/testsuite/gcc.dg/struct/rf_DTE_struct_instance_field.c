// escape_instance_field, "Type escapes via a field of instance".
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

typedef struct network
{
  arc_p arcs;
  arc_p sorted_arcs;
  int x;
  node_p nodes;
  node_p stop_nodes;
  node_t node;
} network_t;


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
  network_t* net_add;
  node_t node;
};


const int MAX = 100;

/* let it escape_array, "Type is used in an array [not handled yet]".  */
network_t* net[2];

int
main ()
{
  net[0] = (network_t*) calloc (1, sizeof(network_t));
  net[0]->arcs = (arc_p) calloc (MAX, sizeof (arc_t));

  /* Contains an escape type and has structure instance field.  */
  net[0]->arcs->node = net[0]->node;

  return 0;
}

/* { dg-final { scan-ipa-dump "No structures to transform." "struct_layout" } } */