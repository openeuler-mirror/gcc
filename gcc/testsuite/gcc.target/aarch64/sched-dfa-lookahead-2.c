/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -mcpu=hip12 -msched-dfa-lookahead=0 -fsched-verbose=2 -fdump-rtl-sched2" } */

void
f (long *restrict out, const long *restrict in)
{
  out[0] = in[0] + 1;
  out[1] = in[1] + 2;
  out[2] = in[2] + 3;
  out[3] = in[3] + 4;
}

/* { dg-final { scan-rtl-dump "DFA lookahead: 0\n" "sched2" } } */
/* { dg-final { scan-rtl-dump-not "max_issue among" "sched2" } } */
