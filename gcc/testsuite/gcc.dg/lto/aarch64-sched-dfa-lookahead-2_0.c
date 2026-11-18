/* { dg-lto-do link } */
/* { dg-skip-if "AArch64 only" { ! aarch64*-*-* } } */
/* { dg-lto-options { { -O2 -flto -flto-partition=one -mtune=hip12 -fno-tree-vectorize -msched-dfa-lookahead=0 -fsched-verbose=2 -fdump-rtl-sched2 } } } */
/* { dg-extra-ld-options "-msched-dfa-lookahead=2" } */

/* The link-time value must reenable the search disabled in the LTO object.  */
__attribute__((noipa)) void
f (long *restrict out, const long *restrict in)
{
  out[0] = in[0] + 1;
  out[1] = in[1] + 2;
  out[2] = in[2] + 3;
  out[3] = in[3] + 4;
}

int
main (int argc, char **argv)
{
  long in[4] = { argc, argc, argc, argc };
  long out[4];
  f (out, in);
  return out[0] & 1;
}

/* { dg-final { scan-dump "ltrans-rtl" "DFA lookahead: 2\n" {[0-9][0-9][0-9]r.sched2} ".ltrans0.ltrans" } } */
