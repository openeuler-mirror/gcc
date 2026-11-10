/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-march=opteron-sse3 -Ofast --param l1-cache-line-size=3 -Wdisabled-optimization" } */
/* { dg-require-effective-target indirect_jumps } */

extern void abort (void);

/* The GCC builtins require five pointer-sized slots, not a libc jmp_buf.  */
__INTPTR_TYPE__ buf[5];

void raise0(void)
{
  __builtin_longjmp (buf, 1);
}

int execute(int cmd) /* { dg-warning "'l1-cache-line-size' parameter is not a power of two 3" } */
{
  int last = 0;

  if (__builtin_setjmp (buf) == 0)
    while (1)
      {
	last = 1;
	raise0 ();
      }

  if (last == 0)
    return 0;
  else
    return cmd;
}

int execute2(int cmd, int cmd2)
{
  int last = 0;

  if (__builtin_setjmp (buf) == 0)
    while (1)
      {
	last = 1;
	raise0 ();
      }

  if (last == 0)
    return 0;
  else
    return cmd;
}


int main(void)
{
  if (execute (1) == 0)
    abort ();

  if (execute2 (1, 2) == 0)
    abort ();

  return 0;
}
