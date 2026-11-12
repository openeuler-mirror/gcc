/* { dg-lto-do run } */
/* { dg-lto-options { { -O3 -flto -fipa-array-load-inline -fno-ipa-cp -fno-ipa-cp-clone -fchecking=2 -flto-partition=one } { -O3 -flto -fipa-array-load-inline -fno-ipa-cp -fno-ipa-cp-clone -fchecking=2 -flto-partition=balanced } } } */

/* LTRANS must read the actual guard initializer before proving a load safe.  */
static int ready[4] = { 0, 0, 0, 0 };
static int tbl[16];

static void __attribute__((noinline))
init (void)
{
  for (int i = 0; i < 16; ++i)
    tbl[i] = i * 3 + 7;
  ready[0] = 1;
}

static int __attribute__((noinline))
get (int i)
{
  if (ready[0])
    return tbl[i];
  return -1;
}

int
main (void)
{
  if (get (2) != -1)
    __builtin_abort ();
  init ();
  if (get (2) != 13)
    __builtin_abort ();
  return 0;
}
