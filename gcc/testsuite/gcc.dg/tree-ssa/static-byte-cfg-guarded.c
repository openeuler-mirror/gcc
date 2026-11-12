/* { dg-do run } */
/* { dg-options "-O2 -ftree-static-byte-cfg -fchecking=2 -fdump-tree-static-byte-cfg-details" } */

#include "static-byte-cfg-guarded.h"

/* { dg-final { scan-tree-dump "GUARDPLAN guarded guarded=1 .*status=ready" "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-times "RESIDUAL-EMIT guarded " 1 "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT reference " "static-byte-cfg" } } */
