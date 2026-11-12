/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-generate=cspgo-err-2.d -fcfgo-csprofile-generate=cspgo-err-2.d" } */

/* The two profile directories must differ.  -fprofile-generate= also does
   not satisfy the "use cspgo together with pgo" check, which asks for a
   profile-use option, so that message is expected here as well.  Both are reported once.  */

int f (void) { return 0; }

/* { dg-error "pgo and cspgo path must different" "" { target *-*-* } 0 } */
/* { dg-error "cspgo must used with cfgo-pgo or pgo" "" { target *-*-* } 0 } */
/* { dg-prune-output "confused by earlier errors" } */
