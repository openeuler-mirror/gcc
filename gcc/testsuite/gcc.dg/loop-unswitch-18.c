/* { dg-do run } */
/* { dg-options "-O2 -funswitch-loops -fno-thread-jumps -fdump-tree-unswitch-details" } */

volatile int effects;

__attribute__ ((noinline, noclone))
static int
record (int value)
{
  effects++;
  return value;
}

__attribute__ ((noipa))
static int
signed_switch (int selector)
{
  int result = 0;

  for (int i = 0; i != 8; ++i)
    switch (selector)
      {
      case -3 ... -1:
	result += record (i);
	break;
      case 0:
	result += 7;
	/* Fall through.  */
      case 2:
	result += record (2 * i);
	break;
      default:
	result -= record (i);
	break;
      }

  return result;
}

__attribute__ ((noipa))
static unsigned
unsigned_switch (unsigned selector)
{
  unsigned result = 0;

  for (unsigned i = 0; i != 8; ++i)
    switch (selector)
      {
      case 0:
	result += record (3);
	break;
      case 1 ... 3:
	result += record (i + 1);
	break;
      case (unsigned) -1:
	result += record (2);
	break;
      default:
	result += record (5);
	break;
      }

  return result;
}

static void
check_signed (int selector, int expected)
{
  effects = 0;
  if (signed_switch (selector) != expected || effects != 8)
    __builtin_abort ();
}

static void
check_unsigned (unsigned selector, unsigned expected)
{
  effects = 0;
  if (unsigned_switch (selector) != expected || effects != 8)
    __builtin_abort ();
}

int
main (void)
{
  check_signed (-4, -28);
  check_signed (-3, 28);
  check_signed (-1, 28);
  check_signed (0, 112);
  check_signed (1, -28);
  check_signed (2, 56);

  check_unsigned (0, 24);
  check_unsigned (1, 36);
  check_unsigned (3, 36);
  check_unsigned (4, 40);
  check_unsigned ((unsigned) -1, 16);
  return 0;
}

/* Verify that the runtime checks exercise switch-aware unswitching.  */
/* { dg-final { scan-tree-dump "unswitching loop . on .switch. with condition" "unswitch" } } */
