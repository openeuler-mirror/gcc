/* { dg-do run } */
/* { dg-options "-O3 -ftree-constant-byte-range-cursor -fdump-tree-constbyterange-details -fwrapv-pointer" } */
#include "constant-byte-cursor-1.c"
/* Wrapping pointer arithmetic disables the entire pass.  */
