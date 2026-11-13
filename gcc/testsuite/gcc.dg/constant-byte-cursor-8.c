/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-constant-byte-range-cursor -fdump-tree-constbyterange-details -ftree-constant-byte-range" } */
/* { dg-final { scan-tree-dump-not "Constant byte cursor range" "constbyterange1" } } */
#include "constant-byte-cursor-1.c"
