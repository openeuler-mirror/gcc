/* { dg-lto-do run } */
/* { dg-lto-options { { -O3 -flto -flto-partition=one -fipa-array-load-inline -fno-ipa-cp -fno-ipa-cp-clone -fchecking=2 } { -O3 -flto -flto-partition=balanced -fipa-array-load-inline -fno-ipa-cp -fno-ipa-cp-clone -fchecking=2 } { -O3 -flto -flto-partition=max -fipa-array-load-inline -fno-ipa-cp -fno-ipa-cp-clone -fchecking=2 } } } */

extern unsigned tbl[16];
extern void init (void);
static unsigned __attribute__((noinline))
read_tbl (unsigned i)
{
  return tbl[i];
}
int main (void)
{
  init ();
  for (unsigned i = 0; i < 16; ++i)
    if (read_tbl (i) != (i * 11 + 7))
      __builtin_abort ();
  return 0;
}
