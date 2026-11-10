/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

#pragma GCC push_options
#pragma GCC optimize ("no-preserve-vect-idioms")
#define NAME disabled
#include "preserve-vect-idioms.h"
#pragma GCC pop_options

#define NAME enabled_again
#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-times "preserving widened multiply reduction" 1 "cunrolli" } } */
/* { dg-final { scan-tree-dump "loop with 16 iterations completely unrolled" "cunrolli" } } */
