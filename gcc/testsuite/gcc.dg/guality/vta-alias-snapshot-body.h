/* Candidate guality coverage shared by the default -Og and -O2 tests.
   Intra-callee caller-frame checks are in the standalone GDB matrix, since
   guality.h always examines the immediate caller of guality_check.  */
#include "guality.h"

static int *vta_escaped;
volatile int vta_snapshot_sink;

__attribute__((noipa)) static void
vta_escape (int *p)
{
  vta_escaped = p;
}

__attribute__((noipa)) static void
vta_change (int replacement)
{
  *vta_escaped = replacement;
}

__attribute__((noipa)) static void
vta_keep_snapshot (int snapshot, int expected)
{
  if (snapshot != expected)
    abort ();
  vta_snapshot_sink = snapshot;
}

__attribute__((noipa)) static void
vta_alias_snapshot (int n)
{
  int b = -3;
  vta_escape (&b);
  if (n > 0)
    b = 5;
  /* Materialize a common-point MEM definition before the scalar read.
     Do not split the snapshot debug binding across constant/MEM paths.  */
  __asm__ volatile ("" : "+m" (b));
  int snapshot = b;
  int expected = n > 0 ? 5 : -3;
  vta_change (37);
  /* This expression string does not cause a new generated-code load of B.
     Unavailable is safe; an available pre-call value is always wrong.  */
  GUALCHKXPRVAL ("b", 37, 1);
  /* The independent snapshot must not be erased to make B safe.  */
  GUALCHKXPRVAL ("snapshot", expected, 0);
  vta_keep_snapshot (snapshot, expected);
}

__attribute__((noipa)) static void
vta_loop_shadow (int n)
{
  int b = n > 0 ? 5 : -3;
  for (int i = 0; i != 3; ++i)
    {
      int expected = i == 0 ? (n > 0 ? 5 : -3) : 79 + i;
      __asm__ volatile ("" : "+m" (b));
      int snapshot = b;
      {
        int b = -11 - i;
        vta_escape (&b);
        vta_change (60 + i);
        GUALCHKXPRVAL ("b", 60 + i, 1);
        GUALCHKXPRVAL ("snapshot", expected, 0);
        vta_keep_snapshot (snapshot, expected);
      }
      GUALCHKXPRVAL ("b", expected, 1);
      vta_escape (&b);
      vta_change (80 + i);
      GUALCHKXPRVAL ("b", 80 + i, 1);
      GUALCHKXPRVAL ("snapshot", expected, 0);
      vta_keep_snapshot (snapshot, expected);
    }
}

int
main (int argc, char **argv)
{
  vta_alias_snapshot (-3);
  vta_alias_snapshot (1);
  vta_loop_shadow (-3);
  vta_loop_shadow (1);
  return 0;
}
