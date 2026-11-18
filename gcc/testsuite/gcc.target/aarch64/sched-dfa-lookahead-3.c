/* { dg-do compile } */
/* { dg-options "-msched-dfa-lookahead=9" } */
/* { dg-error "argument to .-msched-dfa-lookahead=. is not between 0 and 8" "" { target *-*-* } 0 } */

void
f (void)
{
}
