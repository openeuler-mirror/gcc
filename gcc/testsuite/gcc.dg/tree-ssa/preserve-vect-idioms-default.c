/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cunrolli-details" } */

#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "loop with 16 iterations completely unrolled" "cunrolli" } } */
