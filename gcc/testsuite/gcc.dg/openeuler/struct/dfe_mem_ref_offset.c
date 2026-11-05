/* Supports the MEM_REF offset.
   _1 = MEM[(struct arc *)ap_4 + 72B].flow;
   Old rewrite：_1 = ap.reorder.0_8->flow;
   New rewrite：_1 = MEM[(struct arc.reorder.0 *)ap.reorder.0_8 + 64B].flow.  */
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
main ()
{
  const int MAX = 100;
  /* A similar scenario can be reproduced only by using local variables.  */
  arc_p ap = NULL;
  ap = (arc_p) calloc(MAX, sizeof(arc_t));
  printf("%d\n", ap[1].flow);
  return 0; 
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 2 "struct_reorg" } } */
