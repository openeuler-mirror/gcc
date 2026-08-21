/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

typedef struct arc arc_t;
typedef struct arc *arc_p;

typedef struct network
{    
  int x;
} network_t;

struct arc
{
  int flow;
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
  stop_arcs = (arc_p) calloc (MAX, sizeof (arc_t));

  net[0]->x = 100;

  for (unsigned i = 0; i < 3; i++)
    {        
      net[0]->x = net[0]->x + 2;
      stop_arcs->flow = net[0]->x / 2;
      stop_arcs->flow = stop_arcs->flow + 20;
      stop_arcs->net_add = net[0];
      stop_arcs++;
    }

  if( net[1] != 0 && stop_arcs != 0)
    {
      return -1;
    }
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 0 "struct_reorg" } } */
