/* { dg-do run } */
/* { dg-options "-O3 -fdelete-null-pointer-checks -ftree-constant-byte-range-cursor -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump-not "Constant byte cursor range" "constbyterange1" } } */
/* { dg-final { scan-tree-dump-not "Constant byte cursor range" "constbyterange2" } } */
#define PLAIN
#include "constant-byte-cursor-17.c"
