/* { dg-do run } */
/* { dg-options "-O3 -fdelete-null-pointer-checks -ftree-constant-byte-range -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump "Constant byte cursor range" "constbyterange1" { target { ! keeps_null_pointer_checks } } } } */
#include "constant-byte-cursor-1.c"
