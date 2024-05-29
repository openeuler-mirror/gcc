/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -fllc-allocate -fdump-tree-llc_allocate-details-lineno -c --param=mem-access-ratio=1 --param=mem-access-num=0" } */

/* In this deja test case, we test how Phase 2 & 3 of llc-allocate pass deals
   with an indirect memory access in a nested loop where the use-block for the
   induction variable of this memory access is a child/descendent of its
   def-block (we make it by defining the induction variable in the outer loop).
   Therefore, the reference can be successfully traced after outer-loop
   analysis.  */
#include <stdlib.h>
#include <time.h> 

void cross_bb_indir_mem_acc (int *arr1, int *arr2, int *arr3, int *arr4, int n) {
    srand (time (NULL));

    int j_s;
    int j_e = arr1[0];
    int k;

    for (int i = 0; i < n; i++)
    {
        j_s = j_e;
        j_e = arr1[i + 1];

        k = arr3[i];

        for (int j = j_s; j < j_e; j++)
        {
           arr4[j] -= arr2[k];
        }

    }
}

/* { dg-final { scan-tree-dump "Unhandled indirect memory access tracing." "llc_allocate" } } */
/* { dg-final { scan-tree-dump "Retrace indirect memory access after outer loop analysis:" "llc_allocate" } } */
