/* { dg-do run } */
/* { dg-options "-O3 -ftree-constant-byte-range -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump "Constant byte cursor range" "constbyterange1" } } */
#include "constant-byte-cursor-1.c"
