/* { dg-do link } */
/* { dg-options "-O3 -fipa-prefetch -flto -flto-partition=one -fdump-ipa-ipa_prefetch" } */
/* { dg-require-effective-target lto } */

/* Based on opensource gcc code.  */

#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>

#define SPARSESET_ELT_TYPE unsigned int
#define ALLOCNO_NUM(A) ((A)->num)

typedef struct sparseset_def
{
  SPARSESET_ELT_TYPE *dense;	/* Dense array.  */
  SPARSESET_ELT_TYPE *sparse;	/* Sparse array.  */
  SPARSESET_ELT_TYPE members;	/* Number of elements.  */
  SPARSESET_ELT_TYPE size;	/* Maximum number of elements.  */
  SPARSESET_ELT_TYPE iter;	/* Iterator index.  */
  unsigned char iter_inc;	/* Iteration increment amount.  */
  bool iterating;
  SPARSESET_ELT_TYPE elms[2];   /* Combined dense and sparse arrays.  */
} *sparseset;

struct ira_allocno
{
  /* The allocno order number starting with 0.  Each allocno has an
     unique number and the number is never changed for the
     allocno.  */
  int num;
  /* Regno for allocno or cap.  */
  int regno;
  /*...*/
};

typedef struct ira_allocno_live_range *allocno_live_range_t;
typedef struct ira_allocno *ira_allocno_t;

struct ira_allocno_live_range
{
  /* Allocno whose live range is described by given structure.  */
  ira_allocno_t allocno;
  /* Program point range.  */
  int start, finish;
  /* Next structure describing program points where the allocno
     lives.  */
  allocno_live_range_t next;
  /* Pointer to structures with the same start/finish.  */
  allocno_live_range_t start_next, finish_next;
};

bool
sparseset_bit_p (sparseset s, SPARSESET_ELT_TYPE e)
{
  SPARSESET_ELT_TYPE idx;

  idx = s->sparse[e];

  return idx < s->members && s->dense[idx] == e;
}

bool new_pseudos_p;
int ira_max_point, ira_allocnos_num;
allocno_live_range_t *ira_finish_point_ranges;

static inline void
sparseset_clear (sparseset s)
{
  s->members = 0;
  s->iterating = false;
}

sparseset
sparseset_alloc (SPARSESET_ELT_TYPE n_elms)
{
  unsigned int n_bytes = sizeof (struct sparseset_def)
			 + ((n_elms - 1) * 2 * sizeof (SPARSESET_ELT_TYPE));

  /* We use xcalloc rather than xmalloc to silence some valgrind uninitialized
     read errors when accessing set->sparse[n] when "n" is not, and never has
     been, in the set.  These uninitialized reads are expected, by design and
     harmless.  If this turns into a performance problem due to some future
     additional users of sparseset, we can revisit this decision.  */
  sparseset set = (sparseset) calloc (1, n_bytes);
  set->dense = &(set->elms[0]);
  set->sparse = &(set->elms[n_elms]);
  set->size = n_elms;
  sparseset_clear (set);
  return set;
}

void
sparseset_insert_bit (sparseset s, SPARSESET_ELT_TYPE e, SPARSESET_ELT_TYPE idx)
{
  s->sparse[e] = idx;
  s->dense[idx] = e;
}

void
sparseset_swap (sparseset s, SPARSESET_ELT_TYPE idx1, SPARSESET_ELT_TYPE idx2)
{
  SPARSESET_ELT_TYPE tmp = s->dense[idx2];
  sparseset_insert_bit (s, s->dense[idx1], idx2);
  sparseset_insert_bit (s, tmp, idx1);
}

void __attribute__ ((noinline))
sparseset_clear_bit (sparseset s, SPARSESET_ELT_TYPE e)
{
  if (sparseset_bit_p (s, e))
    {
      SPARSESET_ELT_TYPE idx = s->sparse[e];
      SPARSESET_ELT_TYPE iter = s->iter;
      SPARSESET_ELT_TYPE mem = s->members - 1;

      /* If we are iterating over this set and we want to delete a
	 member we've already visited, then we swap the element we
	 want to delete with the element at the current iteration
	 index so that it plays well together with the code below
	 that actually removes the element.  */
      if (s->iterating && idx <= iter)
	{
	  if (idx < iter)
	    {
	      sparseset_swap (s, idx, iter);
	      idx = iter;
	    }
	  s->iter_inc = 0;
	}

      /* Replace the element we want to delete with the last element
	 in the dense array and then decrement s->members, effectively
	 removing the element we want to delete.  */
      sparseset_insert_bit (s, s->dense[mem], idx);
      s->members = mem;
    }
}

allocno_live_range_t r;
sparseset allocnos_live;

void
ira_flattening ()
{
  int i;

  if (new_pseudos_p)
    {
      allocnos_live = sparseset_alloc (ira_allocnos_num);
      for (i = 0; i < ira_max_point; i++)
	{
	  for (r = ira_finish_point_ranges[i]; r != NULL; r = r->finish_next)
	    sparseset_clear_bit (allocnos_live, ALLOCNO_NUM (r->allocno));
	}
    }
}

int main()
{
  ira_flattening ();
  return 0;
}

/* { dg-final { scan-wpa-ipa-dump-times "Insert page check" 1 "ipa_prefetch"} } */
/* { dg-final { scan-wpa-ipa-dump-times "Insert 0 prefetch stmt:" 1 "ipa_prefetch"} } */
/* { dg-final { scan-wpa-ipa-dump-times "Split dom_bb after condition stmts:" 1 "ipa_prefetch"} } */
