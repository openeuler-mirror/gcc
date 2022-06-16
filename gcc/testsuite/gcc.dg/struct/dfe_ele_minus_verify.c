// verify newarc[cmp-1].flow
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

const int MAX = 100;
arc_p ap = NULL;

int
main ()
{
  ap = (arc_p) calloc(MAX, sizeof(arc_t));
  printf("%d\n", ap[0].id);
  for (int i = 1; i < MAX; i++)
    {
      ap[i-1].id = 500;
    }
  printf("%d\n", ap[0].id);
  return 0; 
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 2 "struct_layout" } } */
