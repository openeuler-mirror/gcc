/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -fno-tree-immutable-loop-specialization-cursor -fno-ipa-icf -ftree-immutable-loop-specialization -fdump-tree-cursor-thread-details" } */
#include "immutable-cursor-options.h"
/* { dg-final { scan-tree-dump "Function cursor_enabled" "cursor-thread1" } } */
/* { dg-final { scan-tree-dump "Immutable cursor CFG changed: yes" "cursor-thread1" } } */
/* { dg-final { scan-tree-dump-not "Function cursor_disabled" "cursor-thread1" } } */
/* { dg-final { scan-tree-dump-not "Function cursor_plain" "cursor-thread1" } } */
