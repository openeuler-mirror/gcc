/* { dg-do run } */
/* { dg-options "-O2 -fchecking=2 -fdump-tree-static-byte-cfg-details" } */

#define PROBE_ATTRIBUTE __attribute__ ((optimize ("tree-static-byte-cfg")))
#include "static-byte-cfg-guarded.h"

/* { dg-final { scan-tree-dump-times "RESIDUAL-EMIT guarded " 1 "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT plain_guarded " "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT reference " "static-byte-cfg" } } */
