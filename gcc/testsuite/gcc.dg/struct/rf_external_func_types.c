/* { dg-do compile } */
/* { dg-additional-options "-shared" } */

#include <stdio.h>
#include <stdlib.h>

typedef struct node node_t;
typedef struct node *node_p;

typedef struct arc arc_t;
typedef struct arc *arc_p;

typedef struct network
{    
  int x;
  arc_p arcs, sorted_arcs;  
  node_p nodes, stop_nodes;
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
};

extern int bcf_sr_add_reader (network_t *);
extern int bcf_hdr_dup (arc_p);

int
test ()
{
  network_t *net = (network_t *) calloc (1, 20);

  if (!bcf_sr_add_reader(net)) 
    printf("error");
  arc_p arc = net->nodes->basic_arc;
  if(!bcf_hdr_dup(arc))
    {
      return -1;
    }
  return 0;
}

/* { dg-final { scan-ipa-dump "No structures to transform." "reorder_fields" } } */