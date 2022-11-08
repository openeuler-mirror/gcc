// support POINTER_DIFF_EXPR & NOP_EXPR to avoid 
// escape_unhandled_rewrite, "Type escapes via a unhandled rewrite stmt"
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
};

int
main ()
{
  arc_t *old_arcs;
  node_t *node;
  node_t *stop;
  size_t off;
  network_t* net;

  for( ; node->number < stop->number; node++ )
    {
      off = node->basic_arc - old_arcs;
      node->basic_arc = (arc_t *)(net->arcs + off);
    }
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 3" "struct_reorg" } } */