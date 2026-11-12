/* { dg-do run } */
/* { dg-options "-O3 -fgimple -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* Keep the address in the third RHS operand of a GIMPLE assignment.  */
static int tbl[16];
static int other[16];
int *leak;

static void __attribute__((noinline))
init (void)
{
  for (int i = 0; i < 16; ++i)
    tbl[i] = i * 3 + 1;
}

static int __attribute__((noinline))
get (int i)
{
  return tbl[i];
}

__attribute__((noinline)) __GIMPLE (ssa) int *
select_table (int c)
{
  int (*p)[16];
  int (*q)[16];
  int (*t)[16];
  int *r;
  _Bool b;
  __BB(2):
  q_1 = &other;
  t_2 = &tbl;
  b_3 = c_4(D) != 0;
  p_5 = b_3 ? q_1 : t_2;
  r_6 = (int *) p_5;
  return r_6;
}

int __attribute__((noinline))
run (int c, int i)
{
  init ();
  leak = select_table (c);
  return get (i);
}

int __attribute__((noinline))
read_leak (int i)
{
  return leak[i];
}

int
main (void)
{
  if (run (0, 5) != 16 || read_leak (5) != 16)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: address escapes" "array-load-inline" } } */
