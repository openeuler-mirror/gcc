/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-generate" } */
/* { dg-skip-if "PIC, cmodel and tls-dialect change the addressing" { *-*-* } { "-fpic" "-fPIC" "-fpie" "-fPIE" "-mcmodel=*" "-mtls-dialect=trad" } { "" } } */

/* The indirect-call profiling variable is thread-local by default: the
   call site writes it, the callee's stub reads it, and concurrent
   threads must not clobber each other's entry.  This pins the TLS
   access itself, so kernel-pgo-2.c's absence assertion cannot pass
   vacuously - if profiling stopped touching the variable altogether,
   this test goes red first.  The pattern covers the models reachable
   at the skip list's complement (initial/local-exec and tlsdesc);
   -fPIC with -mtls-dialect=trad goes global-dynamic (:tlsgd:) and
   PIC and non-default code models reshape the plain addressing that
   kernel-pgo-2.c asserts, hence the skip - measured, all of them.  */

int f1 (int x) { return x + 1; }
int (*fp) (int) = f1;

int
call_indirect (int x)
{
  return fp (x);
}

/* { dg-final { scan-assembler {(gottprel|tprel|tlsdesc)[^\n]*__gcov_indirect_call} } } */
