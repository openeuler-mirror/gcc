/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fno-ipa-cp-clone -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

/* Bound analysis recursion before modifying any load.  */
static unsigned tbl[4];

static void __attribute__((noinline))
init (void)
{
  for (unsigned i = 0; i < 4; ++i)
    {
      unsigned x = i + 1;
#define STEP x = x * x + i;
#define FOUR STEP STEP STEP STEP
#define SIXTEEN FOUR FOUR FOUR FOUR
#define SIXTY_FOUR SIXTEEN SIXTEEN SIXTEEN SIXTEEN
      SIXTY_FOUR SIXTY_FOUR SIXTY_FOUR SIXTY_FOUR
#undef SIXTY_FOUR
#undef SIXTEEN
#undef FOUR
#undef STEP
      tbl[i] = x;
    }
}

static unsigned __attribute__((noinline))
get (unsigned i)
{
  return tbl[i];
}

int
main (int argc, char **argv)
{
  init ();
  return get ((unsigned) argc & 3);
}

/* { dg-final { scan-ipa-dump "array-load-inline: reject tbl: formula exceeds analysis limits" "array-load-inline" } } */
/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
