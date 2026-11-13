/* { dg-do compile } */
/* { dg-options "-O2 -fdump-passes -fipa-value-specialization -fno-tree-immutable-loop-specialization -fno-tree-immutable-loop-specialization-cursor" } */
/* { dg-message "tree-immutable-loop *: *OFF" "" { target *-*-* } 0 } */
/* { dg-message "tree-immutable-cursor *: *OFF" "" { target *-*-* } 0 } */
/* { dg-prune-output "(ipa-|rtl-|tree-(?!immutable-loop|immutable-cursor)|\\*)" } */
/* { dg-prune-output "\\(null\\)" } */

/* Both options are disabled.  Pass listing uses a dummy function without
   a CFG, and must still list the two passes without scanning candidates.  */
int
cursor_dump_passes (int x)
{
  return x + 1;
}
