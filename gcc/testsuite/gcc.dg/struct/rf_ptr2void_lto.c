// escape_cast_void, "Type escapes a cast to/from void*"
// stop_393 = net.stop_nodes; void *stop;
/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

typedef struct node node_t;
typedef struct node *node_p;

typedef struct arc arc_t;
typedef struct arc *arc_p;

typedef struct network
{    
  arc_p arcs, sorted_arcs;
  int x;
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

const int MAX = 100;
network_t* net = NULL;
int cnt = 0;

__attribute__((noinline)) int
primal_feasible (network_t *net)
{
  void* stop;
  node_t *node;

  node = net->nodes;
  stop = (void *)net->stop_nodes;
  for( node++; node < (node_t *)stop; node++ )
    {
      printf( "PRIMAL NETWORK SIMPLEX: " );
    }
  return 0;
}

int
main ()
{
  net = (network_t*) calloc (1, 20);
  net->nodes = calloc (MAX, sizeof (node_t));
  net->stop_nodes = calloc (MAX, sizeof (node_t));
  cnt = primal_feasible( net ); 
    
  net = (network_t*) calloc (1, 20); 
  if( !(net->arcs) )
    {
      return -1;
    }
  return cnt;
}

/* { dg-final { scan-ipa-dump "No structures to transform." "reorder_fields" } } */