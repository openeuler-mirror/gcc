! { dg-do compile { target aarch64*-*-* } }
! { dg-options "-O3 -fsimdmath" }
! { dg-require-effective-target simdmath_vectorizes }
! The vector variants asserted below come from the program's own
! !$omp declare simd, which -fno-openmp-simd leaves dormant.  The
! simdmath_vectorizes probe cannot speak for this: it measures a**b,
! which reaches the variants through !GCC$ builtin and is not gated by
! that option, so it answers yes on a board where these cannot hold.
! { dg-skip-if "needs omp declare simd active" { *-*-* } { "-fno-openmp-simd" } { "" } }
!
! The clone gate applies only to bare declarations, whose implementations
! come from the vector math library.  A function DEFINED here is a
! different matter: its clones are its ABI, so it keeps the full upstream
! AArch64 set including the 64-bit V2SF variant.  Narrowing the gate to
! definitions as well would make callers compiled without -fsimdmath ask
! for a _ZGVnN2v_ symbol this translation unit no longer provides.
!
! myd is a 128-bit V2DF clone, unaffected by the gate either way, so it
! separates this from the question of whether declare simd works at all.

function myf (x)
  real(4) :: myf, x
  !$omp declare simd(myf) notinbranch
  myf = x * 2.0 + 1.0
end function

function myd (x)
  real(8) :: myd, x
  !$omp declare simd(myd) notinbranch
  myd = x * 2.0d0 + 1.0d0
end function

! { dg-final { scan-assembler {_ZGVnN2v_myf} } }
! { dg-final { scan-assembler {_ZGVnN2v_myd} } }
