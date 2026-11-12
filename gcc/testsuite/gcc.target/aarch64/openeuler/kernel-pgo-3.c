/* { dg-do link } */
/* { dg-options "-O2 -fprofile-generate -fkernel-pgo" } */

/* Misusing the option in user space must fail loud, not misbehave:
   libgcov defines __gcov_indirect_call thread-locally (wherever the
   target has TLS), an object built with -fkernel-pgo refers to it as a
   plain global, and the linker refuses to join the two.  This pins
   that the failure stays a hard, self-explanatory link error - the
   only thing standing between a wrong option and silently corrupted
   profiling.

   Pin the reason, not just the failure: a bare dg-excess-errors passes
   on any link error at all.  The linker's message is attributed to no
   line, so dg-regexp matches the output directly and dg-excess-errors
   absorbs the rest, the same shape as simdmath-link-c-1.c.  */

int f1 (int x) { return x + 1; }
int (*fp) (int) = f1;

int
main (void)
{
  return fp (1) - 2;
}

/* { dg-regexp {[^\n]*__gcov_indirect_call[^\n]*TLS definition[^\n]*mismatches non-TLS reference[^\n]*} } */
/* { dg-excess-errors "the TLS/non-TLS link mismatch is the point" } */
