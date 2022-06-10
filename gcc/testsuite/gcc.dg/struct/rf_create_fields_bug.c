// bugfix: 
// Common members do not need to reconstruct. 
// Otherwise, eg:int* -> int** and void* -> void**.
/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

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
  int64_t* cost;
  node_p tail;
  node_p head;
  short ident;
  arc_p nextout;
  arc_p nextin;
  int64_t flow;
  int64_t** org_cost;
};

struct a
{
  int t;
  int t1;
};

__attribute__((noinline)) int
f(int i, int j)
{
  struct a *t = NULL;
  struct a t1 = {i, j};
  t = &t1;
  auto int g(void) __attribute__((noinline));
  int g(void)
    {
      return t->t + t->t1;
    }
  return g();
}

arc_t **ap = NULL;
const int MAX = 100;

int
main()
{
  if (f(1, 2) != 3)
    {
      abort ();
    }
  ap = (arc_t**) malloc(MAX * sizeof(arc_t*));
  (*ap)[0].id = 300;
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 2" "struct_layout" } } */