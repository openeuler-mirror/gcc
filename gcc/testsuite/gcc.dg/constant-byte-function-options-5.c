/* { dg-do compile } */
/* { dg-options "-O3 -fdelete-null-pointer-checks -fno-tree-constant-byte-range -fno-tree-constant-byte-range-cursor -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump "Function byte_range_option_probe " "constbyterange1" } } */
/* { dg-final { scan-tree-dump "Function byte_range_option_probe " "constbyterange2" } } */
/* { dg-final { scan-tree-dump "Constant byte cursor range" "constbyterange1" { target { ! keeps_null_pointer_checks } } } } */
#define PROBE_ATTRIBUTES noipa, optimize ("tree-constant-byte-range-cursor")
#include "constant-byte-function-options.h"
