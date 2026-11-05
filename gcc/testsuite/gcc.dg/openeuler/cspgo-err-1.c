/* { dg-do compile } */
/* { dg-options "-O2 -fcfgo-csprofile-generate" } */

/* Context-sensitive PGO validates its arguments in the pass gate and
   reports every problem it finds.  Asking for it with no profile
   directories and no PGO must report both.  */

int f (void) { return 0; }

/* { dg-error "pgo profile path must set when using cspgo" "" { target *-*-* } 0 } */
/* { dg-error "cspgo must used with cfgo-pgo or pgo" "" { target *-*-* } 0 } */
/* { dg-prune-output "confused by earlier errors" } */
