/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli-details" } */

#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump-not "completely unrolled" "cunrolli" } } */
