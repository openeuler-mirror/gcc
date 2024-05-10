// Check simplify rewrite chance for semi-relayout
/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

typedef struct node node_t;
typedef struct node *node_p;

typedef struct arc arc_t;
typedef struct arc *arc_p;

typedef struct network
{
  arc_p arcs;
  arc_p sorted_arcs;
  int x;
  node_p nodes;
  node_p stop_nodes;
} network_t;

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
  network_t* net_add;
};


const int MAX = 100;
network_t* net;
node_p node;
arc_p arc;

int
main ()
{
  net = (network_t*) calloc (1, sizeof(network_t));
  net->arcs = (arc_p) calloc (MAX, sizeof (arc_t));
  net->sorted_arcs = (arc_p) calloc (MAX, sizeof (arc_t));
  net->nodes = (node_p) calloc (MAX, sizeof (node_t));
  net->arcs->id = 100;

  node = net->nodes;
  arc = net->arcs;

  for (unsigned i = 0; i < MAX; i++)
    {
      arc->head = node;
      arc->head->child = node;
      node->potential = i + 1;
      arc->cost = arc->head->potential;
      arc->tail = node->sibling;
      arc->head = arc->tail;
      node = node + 1;
      arc = arc + 1;
    }

  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform in semi-relayout is 1" "struct_reorg" } } */