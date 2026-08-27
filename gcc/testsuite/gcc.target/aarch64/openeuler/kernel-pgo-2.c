/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-generate -fkernel-pgo" } */
/* { dg-skip-if "PIC, cmodel and tls-dialect change the addressing" { *-*-* } { "-fpic" "-fPIC" "-fpie" "-fPIE" "-mcmodel=*" "-mtls-dialect=trad" } { "" } } */

/* -fkernel-pgo strips the TLS model from the indirect-call profiling
   variable: a kernel has no ELF TLS runtime and its loader does not
   process TLS relocations, so the variable has to be addressed as a
   plain global there.  Both halves are asserted - no TLS access, and
   the plain :lo12: access present - so this cannot pass by profiling
   simply not referencing the variable at all.  kernel-pgo-1.c pins the
   default behaviour this one must differ from, and carries the same
   skip: under PIC or a non-default code model the plain access is
   spelled :got_lo12:, :gotpage_lo15:, adr or a literal pool word
   instead - measured - and no one pattern covers them all.  */

int f1 (int x) { return x + 1; }
int (*fp) (int) = f1;

int
call_indirect (int x)
{
  return fp (x);
}

/* { dg-final { scan-assembler-not {(gottprel|tprel|tlsdesc)[^\n]*__gcov_indirect_call} } } */
/* { dg-final { scan-assembler {:lo12:__gcov_indirect_call} } } */
