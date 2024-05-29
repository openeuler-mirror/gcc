/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -fllc-allocate -fdump-tree-llc_allocate-details-lineno -c --param=force-issue=1" } */

/* In this deja test case, we test how Phase 4 of llc-allocate pass deals with
   cfg that contains a backedge not being the latch of a formal GCC loop
   structure.  */
typedef unsigned long size_t;
typedef long scalar_t__;

typedef  struct TYPE_13__   TYPE_3__ ;
typedef  struct TYPE_12__   TYPE_2__ ;
typedef  struct TYPE_11__   TYPE_1__ ;

struct dom_info {int nodes; int* dfs_parent; int* dfs_order; int* key; int* next_bucket; int* bucket; int* dom; int fake_exit_edge; TYPE_3__** dfs_to_bb; } ;
typedef  enum cdi_direction { ____Placeholder_cdi_direction } cdi_direction ;
struct TYPE_11__ {scalar_t__ index; } ;
typedef  TYPE_1__ edge_iterator ;
typedef  TYPE_2__* edge ;
typedef  TYPE_3__* basic_block ;
struct TYPE_13__ {size_t index; int preds; int succs; } ;
struct TYPE_12__ {TYPE_3__* src; TYPE_3__* dest; } ;
typedef  int TBB ;

basic_block ENTRY_BLOCK_PTR ;
basic_block EXIT_BLOCK_PTR ;
scalar_t__ bitmap_bit_p (int,size_t) ;
edge ei_edge (edge_iterator) ;
int ei_end_p (edge_iterator) ;
int ei_next (edge_iterator*) ;
edge_iterator ei_start (int) ;
size_t eval (struct dom_info*,int) ;
size_t last_basic_block ;
int link_roots (struct dom_info*,int,int) ;

__attribute__((used)) static void
calc_idoms (struct dom_info *di, enum cdi_direction reverse)
{
  TBB v, w, k, par;
  basic_block en_block;
  edge_iterator ei, einext;

  if (reverse)
    en_block = EXIT_BLOCK_PTR;
  else
    en_block = ENTRY_BLOCK_PTR;

  /* Go backwards in DFS order, to first look at the leafs.  */
  v = di->nodes;
  while (v > 1)
    {
      basic_block bb = di->dfs_to_bb[v];
      edge e;

      par = di->dfs_parent[v];
      k = v;

      ei = (reverse) ? ei_start (bb->succs) : ei_start (bb->preds);

      if (reverse)
	{
	  /* If this block has a fake edge to exit, process that first.  */
	  if (bitmap_bit_p (di->fake_exit_edge, bb->index))
	    {
	      einext = ei;
	      einext.index = 0;
	      goto do_fake_exit_edge;
	    }
	}

      /* Search all direct predecessors for the smallest node with a path
	 to them.  That way we have the smallest node with also a path to
	 us only over nodes behind us.  In effect we search for our
	 semidominator.  */
      while (!ei_end_p (ei))
	{
	  basic_block b;
	  TBB k1;

	  e = ei_edge (ei);
	  b = (reverse) ? e->dest : e->src;
	  einext = ei;
	  ei_next (&einext);

	  if (b == en_block)
	    {
	    do_fake_exit_edge:
	      k1 = di->dfs_order[last_basic_block];
	    }
	  else
	    k1 = di->dfs_order[b->index];

	  /* Call eval() only if really needed.  If k1 is above V in DFS tree,
	     then we know, that eval(k1) == k1 and key[k1] == k1.  */
	  if (k1 > v)
	    k1 = di->key[eval (di, k1)];
	  if (k1 < k)
	    k = k1;

	  ei = einext;
	}

      di->key[v] = k;
      link_roots (di, par, v);
      di->next_bucket[v] = di->bucket[k];
      di->bucket[k] = v;

      /* Transform semidominators into dominators.  */
      for (w = di->bucket[par]; w; w = di->next_bucket[w])
	{
	  k = eval (di, w);
	  if (di->key[k] < di->key[w])
	    di->dom[w] = k;
	  else
	    di->dom[w] = par;
	}
      /* We don't need to cleanup next_bucket[].  */
      di->bucket[par] = 0;
      v--;
    }

  /* Explicitly define the dominators.  */
  di->dom[1] = 0;
  for (v = 2; v <= di->nodes; v++)
    if (di->dom[v] != di->key[v])
      di->dom[v] = di->dom[di->dom[v]];
}

/* { dg-final { scan-tree-dump-times "Warning: Find cycle at bb index" 2 "llc_allocate" } } */
/* { dg-final { scan-tree-dump "static issue" "llc_allocate" } } */
