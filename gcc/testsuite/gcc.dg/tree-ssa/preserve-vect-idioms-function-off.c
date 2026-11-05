/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

#define NAME enabled
#include "preserve-vect-idioms.h"

#define NAME disabled
#define ATTR noipa, optimize ("no-preserve-vect-idioms")
#include "preserve-vect-idioms.h"

#define NAME no_vectorize
#define ATTR noipa, optimize ("no-tree-vectorize")
#include "preserve-vect-idioms.h"

#define NAME enabled_again
#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-times "preserving widened multiply reduction" 2 "cunrolli" } } */
/* { dg-final { scan-tree-dump-times "loop with 16 iterations completely unrolled" 2 "cunrolli" } } */
