/* { dg-do run } */
/* { dg-options "-O3 -ftree-constant-byte-range-cursor -fdump-tree-constbyterange-details -flto -flto-partition=one" } */
/* { dg-final { scan-tree-dump "Constant byte cursor range" "constbyterange1" } } */
#include "constant-byte-cursor-1.c"
