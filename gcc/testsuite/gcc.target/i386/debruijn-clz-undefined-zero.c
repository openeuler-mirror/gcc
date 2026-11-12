/* { dg-do compile } */
/* { dg-options "-O2 -fdebruijn-clz -mno-lzcnt -fdump-tree-forwprop1" } */

/* A possibly-zero input must retain the table lookup when the target
   cannot describe a defined zero result for the internal CLZ call.  */
#include "pr120032-3.c"

/* { dg-final { scan-tree-dump-not {\.CLZ|__builtin_clz} "forwprop1" } } */
