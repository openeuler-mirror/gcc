/* { dg-do compile } */
/* { dg-options "-O2 -fdump-passes -fipa-value-specialization -fno-tree-immutable-loop-specialization -ftree-immutable-loop-specialization-cursor" } */
/* { dg-message "tree-immutable-loop *: *OFF" "" { target *-*-* } 0 } */
/* { dg-message "tree-immutable-cursor *: *OFF" "" { target *-*-* } 0 } */
/* { dg-prune-output "(ipa-|rtl-|tree-(?!immutable-loop|immutable-cursor)|\\*)" } */
/* { dg-prune-output "\\(null\\)" } */

/* Cursor only: this used to scan the dummy function's missing CFG.  */
#include "immutable-cursor-dump-passes-1.c"
