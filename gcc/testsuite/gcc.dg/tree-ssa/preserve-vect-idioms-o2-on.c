/* { dg-do compile } */
/* { dg-options "-O2 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Early unrolling already rejects code growth at -O2.  */
#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-not "completely unrolled" "cunrolli" } } */
/* { dg-final { scan-tree-dump-not "preserving widened multiply reduction" "cunrolli" } } */
/* { dg-final { scan-tree-dump "size would grow" "cunrolli" } } */
