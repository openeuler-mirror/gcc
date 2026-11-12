/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-cunrolli-details" } */

#define NAME disabled
#include "preserve-vect-idioms.h"

#define NAME enabled
#define ATTR noipa, optimize ("preserve-vect-idioms")
#include "preserve-vect-idioms.h"

#define NAME disabled_again
#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-times "preserving widened multiply reduction" 1 "cunrolli" } } */
/* { dg-final { scan-tree-dump-times "loop with 16 iterations completely unrolled" 2 "cunrolli" } } */
