// For testing:
/*
Compile options: gcc -O3 -g 
-flto -flto-partition=one -fipa-reorder-fields -fipa-struct-reorg 
-v -save-temps -fdump-ipa-all-details test.c -o  test

in COMPLETE_STRUCT_RELAYOUT pass：
N type: struct node.reorder.0 new = "Type escapes a cast to a different pointer"
copy$head_26 = test_arc.reorder.0_49->head;

type : struct arc.reorder.0(1599) { 
fields = {
field (5382) {type = cost_t}
field (5383) {type = struct node.reorder.0 *} // but node has escaped.
field (5384) {type = struct node.reorder.0 *}
field (5386) {type = struct arc.reorder.0 *}
field (5387) {type = struct arc.reorder.0 *}
field (5388) {type = flow_t}
field (5389) {type = cost_t}
field (5381) {type = int}
field (5385) {type = short int}
}

// The types of the two types are inconsistent after the rewriting.
newarc_2(D)->tail = tail_1(D);
vs
struct_reorder.0_61(D)->tail = tail_1(D); 
*/
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

__attribute__((noinline)) void
replace_weaker_arc( arc_t *newarc, node_t *tail, node_t *head)
{
    printf("test");
}

__attribute__((noinline)) int64_t
switch_arcs(arc_t** deleted_arcs, arc_t* arcnew)
{
  int64_t count = 0;
  arc_t *test_arc, copy;

  if (!test_arc->ident)
    {
      copy = *test_arc;
      count++;
      *test_arc = arcnew[0];
      replace_weaker_arc(arcnew, NULL, NULL);
    }
  return count;
}

int
main ()
{
  switch_arcs(NULL, NULL);
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 2" "struct_layout" } } */