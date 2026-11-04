/* { dg-do run } */
/* { dg-options "-O3 -ftree-constant-byte-range -fno-tree-constant-byte-range -fdump-tree-constbyterange-details" } */
#include "constant-byte-cursor-1.c"
/* Both options are disabled, so the pass does not produce a dump.  */
