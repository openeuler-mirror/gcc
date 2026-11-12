/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details -fno-preserve-vect-idioms" } */

#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "loop with 16 iterations completely unrolled" "cunrolli" } } */
