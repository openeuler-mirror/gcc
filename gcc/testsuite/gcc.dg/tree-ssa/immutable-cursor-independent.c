/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -fno-tree-immutable-loop-specialization -ftree-immutable-loop-specialization-cursor -fdump-tree-cursor-thread-details" } */
/* { dg-final { scan-tree-dump "Immutable cursor CFG changed: yes" "cursor-thread1" } } */
#include "immutable-cursor-1.c"
