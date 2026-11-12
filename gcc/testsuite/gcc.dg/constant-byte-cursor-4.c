/* { dg-do run } */
/* { dg-options "-O3 -ftree-constant-byte-range-cursor -fdump-tree-constbyterange-details -fno-delete-null-pointer-checks" } */
/* { dg-final { scan-tree-dump-not "Constant byte cursor range" "constbyterange1" } } */
#include "constant-byte-cursor-1.c"
