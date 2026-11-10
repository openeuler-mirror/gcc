/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump-not "completely unrolled" "cunrolli" } } */
