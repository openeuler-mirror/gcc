/* { dg-do compile } */
/* { dg-options "-O2 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Early unrolling already rejects code growth at -O2.  Do not require the
   absence of a preservation message: the decision is what matters.  */
#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-not "completely unrolled" "cunrolli" } } */
