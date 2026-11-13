/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -fwrapv-pointer -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=64 -fdump-tree-linvsplit-details" } */

/* Nonwrapping accesses remain correct with pointer wrapping enabled.  */
#include "linvsplit-pointer-branch.c"
