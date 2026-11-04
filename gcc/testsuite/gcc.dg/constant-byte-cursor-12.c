/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-constant-byte-range -ftree-constant-byte-range-cursor -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump "Constant byte cursor range" "constbyterange1" } } */
#include "constant-byte-cursor-1.c"
