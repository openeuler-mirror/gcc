/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-loop-optimize -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Neither vectorization nor late complete unrolling will run.  */
#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "loop with 16 iterations completely unrolled" "cunrolli" } } */
