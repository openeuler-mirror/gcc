// Verify in escape_dependent_type_escapes,
// the multi-layer dereference is rewriting correctly,and the memory access
// is correct.

// release
// escape_dependent_type_escapes,
// "Type uses a type which escapes or is used by a type which escapes"
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
  network_t* net_add;
};


const int MAX = 100;

/* let it escape_array, "Type is used in an array [not handled yet]".  */
network_t* net[2];
arc_p stop_arcs = NULL;

int
main ()
{
  net[0] = (network_t*) calloc (1, sizeof(network_t));
  net[0]->arcs = (arc_p) calloc (MAX, sizeof (arc_t));
  stop_arcs = (arc_p) calloc (MAX, sizeof (arc_t));

  net[0]->arcs->id = 100;

  for (unsigned i = 0; i < 3; i++)
    {        
      net[0]->arcs->id = net[0]->arcs->id + 2;
      stop_arcs->cost = net[0]->arcs->id / 2;
      stop_arcs->net_add = net[0];
      printf("stop_arcs->cost = %ld\n", stop_arcs->cost);
      net[0]->arcs++;
      stop_arcs++;
    }

  if( net[1] != 0 && stop_arcs != 0)
    {
      return -1;
    }
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 2" "struct_reorg" } } */