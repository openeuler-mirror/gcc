/* { dg-do compile } */
/* { dg-options "-O3 -fdelete-null-pointer-checks -ftree-constant-byte-range -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump-not "Function byte_range_option_probe " "constbyterange1" } } */
/* { dg-final { scan-tree-dump-not "Function byte_range_option_probe " "constbyterange2" } } */
#define PROBE_ATTRIBUTES noipa, optimize ("no-tree-constant-byte-range")
#include "constant-byte-function-options.h"
