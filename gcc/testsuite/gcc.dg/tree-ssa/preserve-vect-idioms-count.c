/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

#define NAME three
#define COUNT 3
#include "preserve-vect-idioms.h"

#define NAME four
#define COUNT 4
#include "preserve-vect-idioms.h"

#define NAME five
#define COUNT 5
#include "preserve-vect-idioms.h"

/* { dg-final { scan-tree-dump-times "preserving widened multiply reduction" 2 "cunrolli" } } */
/* { dg-final { scan-tree-dump-times "completely unrolled" 1 "cunrolli" } } */
