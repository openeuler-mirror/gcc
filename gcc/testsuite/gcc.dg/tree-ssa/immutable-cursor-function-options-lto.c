/* { dg-do run } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O3 -fchecking=2 -fno-ipa-icf -flto -flto-partition=one -ftree-immutable-loop-specialization-cursor -fdump-tree-cursor-thread-details" } */
#include "immutable-cursor-options.h"
/* { dg-final { scan-ltrans-tree-dump "Function cursor_enabled" "cursor-thread1" } } */
/* { dg-final { scan-ltrans-tree-dump "Function cursor_plain" "cursor-thread1" } } */
/* { dg-final { scan-ltrans-tree-dump "Immutable cursor CFG changed: yes" "cursor-thread1" } } */
/* { dg-final { scan-ltrans-tree-dump-not "Function cursor_disabled" "cursor-thread1" } } */
