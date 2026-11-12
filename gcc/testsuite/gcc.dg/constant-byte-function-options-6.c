/* { dg-do run } */
/* { dg-options "-O3 -fdelete-null-pointer-checks -fno-tree-constant-byte-range -fno-tree-constant-byte-range-cursor -flto -flto-partition=max -fdump-tree-constbyterange-details" } */
/* { dg-require-effective-target lto } */
/* { dg-final { scan-dump "ltrans-tree" "Constant byte cursor range" "\[0-9\]\[0-9\]\[0-9\]t.constbyterange1" ".ltrans\[0-9\]*.ltrans" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-dump "ltrans-tree" "Function byte_range_option_probe " "\[0-9\]\[0-9\]\[0-9\]t.constbyterange2" ".ltrans\[0-9\]*.ltrans" } } */
#define PROBE_ATTRIBUTES noipa, optimize ("tree-constant-byte-range-cursor")
#define INCLUDE_MAIN
#include "constant-byte-function-options.h"
