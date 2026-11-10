/* { dg-do run } */
/* { dg-options "-O3 -fdelete-null-pointer-checks -fno-tree-constant-byte-cursor-range -ftree-constant-byte-range -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump-not "Constant byte cursor range" "constbyterange1" } } */
#include "constant-byte-cursor-1.c"
