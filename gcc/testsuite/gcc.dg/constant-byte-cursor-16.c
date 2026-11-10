/* { dg-do run } */
/* { dg-options "-O3 -ftree-constant-byte-range -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump-not "Constant byte cursor range" "constbyterange1" } } */
#define __builtin_strchr __builtin_strrchr
#include "constant-byte-cursor-1.c"
