/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */

#include <stdlib.h>

struct fpm_scoreboard_proc_s {
    int dummy;
};

struct fpm_scoreboard_s {
    struct fpm_scoreboard_proc_s **procs;
};

int __GIMPLE(ssa,startwith("struct_reorg")) main() {
  int i;
  struct fpm_scoreboard_s * scoreboard;
  void * _2;
  void * _3;
  struct fpm_scoreboard_proc_s * * _4;
  struct fpm_scoreboard_proc_s * _5;

__BB(2):
  scoreboard_10 = malloc (8UL);
  goto __BB4;

__BB(3):
  _2 = malloc (8UL);
  scoreboard_10->procs = _2;
  _3 = malloc (4UL);
  __MEM <struct fpm_scoreboard_proc_s *> ((struct fpm_scoreboard_proc_s * *)_2) = _3;
  goto __BB4;

__BB(4):
  i_6 = __PHI (__BB2: 0, __BB3: 1);
  if (i_6 == 0)
    goto __BB3;
  else
    goto __BB5;

__BB(5):
  _4 = scoreboard_10->procs;
  _5 = *_4;
  free (_5);
  free (_4);
  free (scoreboard_10);
  return 0;
}

/* { dg-final { scan-ipa-dump "struct fpm_scoreboard_proc_s has alloc number: -2, skip relayout" "struct_reorg" } } */
