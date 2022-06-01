// release type is used by a type which escapes.
// avoid escape_cast_another_ptr, "Type escapes a cast to a different pointer"
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

const int MAX = 100;
network_t* net = NULL;
arc_p stop_arcs = NULL;
int cnt = 0;

int
main ()
{
  net = (network_t*) calloc (1, 20);
  net->arcs = (arc_p) calloc (MAX, sizeof (arc_t));
  stop_arcs = (arc_p) calloc (MAX, sizeof (arc_t));
  if(!(net->arcs))
    {
      return -1;
    }

  for( int i = 0; i < MAX; i++, net->arcs = stop_arcs)
    {
      cnt++;
    }

  net = (network_t*) calloc (1, 20); 
  if( !(net->arcs) )
    {
      return -1;
    }
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 2" "struct_layout" } } */