// release escape_ptr_ptr, "Type is used in a pointer to a pointer [not handled yet]";
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
arc_t **ap = NULL;

int
main ()
{
  ap = (arc_t**) malloc(MAX * sizeof(arc_t*));
  (*ap)[0].id = 300;
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 2 "reorder_fields" } } */
